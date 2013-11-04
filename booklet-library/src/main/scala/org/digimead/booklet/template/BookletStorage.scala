/**
 * Booklet - a publishing library for short texts.
 *
 * Copyright (c) ... - 2013 Nathan Hamblen & others
 * Copyright (c) 2013 Alexey Aksenov ezh@ezh.msk.ru
 *
 * All rights reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package org.digimead.booklet.template

import java.io.File
import java.io.IOException
import java.util.Properties

import org.digimead.booklet.Booklet
import org.digimead.booklet.Resources
import org.digimead.booklet.Settings
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Leaf
import org.digimead.booklet.content.Section
import org.digimead.booklet.discounter.Discounter
import org.slf4j.LoggerFactory

import com.tristanhunt.knockoff.Block

class BookletStorage(val input: File, val properties: Properties = new Properties) extends Storage {
  protected val log = LoggerFactory.getLogger(getClass)

  if (log.isDebugEnabled())
    Booklet.dump(properties, "User properties(%d):")

  /**
   * Return booklet template.
   * If there is no template then storage consider as plain markdown index.
   */
  def baseBookletProperties: Properties = {
    // use user properties for baseBookletPropertiesFile
    implicit val properties = Booklet.merge(new Properties, this.properties)
    val result = if (baseBookletPropertiesFile.exists() &&
      baseBookletPropertiesFile.isFile() && baseBookletPropertiesFile.canRead())
      // ! this.properties must be merged at last
      Booklet.mergeWithFiles(new Properties, baseBookletPropertiesFile)
    else
      properties

    updateProperties(new BookletStorage.Location(input, Settings.defaultLanguage)(result), result)
    result
  }
  def baseBookletPropertiesFile(implicit properties: Properties) = new File(input, Settings.templateProperties)
  def globalized = {
    implicit val baseProperties = Booklet.merge(baseBookletProperties, properties)
    if (log.isDebugEnabled())
      Booklet.dump(baseProperties, "Base properties(%d):")

    val contents = Settings.languages(baseProperties) map { lang ⇒
      val location = new BookletStorage.Location(input, lang)(baseProperties)
      if (!location.baseLang.isDirectory())
        throw new IOException("Unable to read " + location.baseLang.getCanonicalPath())
      val properties = if (location.isDefault) baseProperties else new File(location.baseLang, Settings.templateProperties) match {
        case file if file.exists() && file.isFile() && file.canRead() ⇒ Booklet.merge(Booklet.mergeWithFiles(baseBookletProperties, file), this.properties)
        case _ ⇒ Booklet.merge(new Properties, baseProperties)
      }
      updateProperties(location, properties)
      val files = location.baseLang.listFiles.filter(_.getName == "files").flatMap(_.listFiles.map { f ⇒ (f.getName, f.toURI) })
      val favicon = location.baseLang.listFiles.filter(_.getName == "favicon.ico").headOption.map { _.toURI }
      if (!Settings.index) {
        log.debug("Process storage as booklets.")
        lang -> Content(location, rootSection(location.baseLang, lang)(properties), files, favicon)
      } else {
        log.debug("Process storage as index of raw markdowns.")
        lang -> Content(location, indexSection(location.baseLang, lang)(properties), files, favicon)
      }
    }
    Globalized(Map(contents: _*))
  }
  def indexSection(dir: File, lang: String)(implicit properties: Properties): Section = {
    def emptySection = Section("", Seq.empty, Nil)
    if (dir.exists)
      indexSection("", dir, lang).headOption getOrElse emptySection
    else
      emptySection
  }
  def rootSection(dir: File, lang: String)(implicit properties: Properties): Section = {
    def emptySection = Section("", Seq.empty, Nil)
    if (dir.exists)
      bookletSection("", dir, lang).headOption getOrElse emptySection
    else
      emptySection
  }
  def read(file: File) = scala.io.Source.fromFile(file).mkString("")
  def knock(file: File)(implicit properties: Properties): (Seq[Block], Properties) = {
    val frontin = Frontin(read(file))
    val markdownProperties = Booklet.mergeWithStrings(properties, frontin.header.toSeq: _*)
    try ((Discounter.knockoff(Printer.process(frontin body)(markdownProperties)), markdownProperties))
    catch {
      case e: Throwable ⇒
        Console.err.println("Error while processing " + file.toString)
        throw e
    }
  }
  def writeTemplates(target: File, lang: String)(implicit properties: Properties) {
    log.debug(s"Write templates to ${target.getCanonicalPath()} for '${lang}'.")
    if (lang == Settings.defaultLanguage)
      Resources.writeTo(Resources.paths(withTemplates = true), target)
    else
      Resources.writeTo(Resources.paths(withTemplates = true), target, lang)
  }

  protected def bookletSection(localPath: String, dir: File, lang: String)(implicit properties: Properties): Option[Section] = {
    val files = getFiles(dir, lang)
    files.find(isMarkdown).map { head ⇒
      val (blocks, markdownProperties) = knock(head)
      val childFiles = files.filterNot { _ == head } filterNot { f ⇒ f.isDirectory && Settings.languages.contains(f.getName) }
      val children = childFiles.flatMap { f ⇒
        if (isMarkdown(f))
          try {
            val (blocks, markdownProperties) = knock(f)
            Seq(Leaf(localPath + "/" + f.getName, blocks)(markdownProperties))
          } catch {
            case e: Throwable ⇒
              log.error(s"Markdown file '${f.getName}' is broken: " + e.getMessage(), e)
              Seq.empty
          }
        else {
          Settings.excludeFolder match {
            case Some(filter) ⇒
              if (!filter.r.pattern.matcher(f.getName).matches)
                bookletSection(localPath + "/" + f.getName, f, lang).toSeq
              else {
                log.debug(s"Skip folder '${f.getName}' because of filter /$filter/")
                Seq.empty
              }
            case None ⇒
              bookletSection(localPath + "/" + f.getName, f, lang).toSeq
          }
        }
      }
      Section(localPath, blocks, children)(markdownProperties)
    }
  }
  protected def indexSection(localPath: String, dir: File, lang: String)(implicit properties: Properties): Option[Section] = {
    val (blocks, markdownProperties) = knock(Settings.indexMarkdownLocation getOrElse
      { throw new IllegalStateException("Unable to find 'indexMarkdownLocation' property.") })
    val files = getFiles(dir, lang)
    files.find(isMarkdown).map { head ⇒
      val childFiles = files.filterNot { f ⇒ f.isDirectory && Settings.languages.contains(f.getName) }
      val children = childFiles.flatMap { f ⇒
        if (isMarkdown(f))
          try {
            val (blocks, markdownProperties) = knock(f)
            Seq(Leaf(localPath + "/" + f.getName, blocks)(markdownProperties))
          } catch {
            case e: Throwable ⇒
              log.error(s"Markdown file '${f.getName}' is broken: " + e.getMessage(), e)
              Seq.empty
          }
        else {
          Settings.excludeFolder match {
            case Some(filter) ⇒
              if (!filter.r.pattern.matcher(f.getName).matches)
                indexSection(localPath + "/" + f.getName, f, lang).toSeq
              else {
                log.debug(s"Skip folder '${f.getName}' because of filter /$filter/")
                Seq.empty
              }
            case None ⇒
              indexSection(localPath + "/" + f.getName, f, lang).toSeq
          }
        }
      }
      Section(localPath, blocks, children)(markdownProperties)
    }
  }
  protected def getFiles(dir: File, lang: String)(implicit properties: Properties): List[File] = {
    val baseDirectoryForLang = new File(input, lang)
    val templateDirectoryName = Settings.template
    Settings.excludeMarkdown match {
      case Some(filter) ⇒
        val rFilter = filter.r
        (Option(dir.listFiles) match {
          case Some(files) ⇒ files.toList.filterNot { f ⇒
            f.isDirectory() && f.getName() == templateDirectoryName &&
              (f.getParentFile.getCanonicalFile() == input.getCanonicalFile() || f.getParentFile.getCanonicalFile() == baseDirectoryForLang) || {
                val skip = isMarkdown(f) && rFilter.findFirstMatchIn(f.getName()).nonEmpty
                if (skip)
                  log.debug(s"Skip markdown '${f.getName}' because of filter /$filter/")
                skip
              }
          }
          case None ⇒ Nil
        }).sortWith(_.getName < _.getName)
      case None ⇒
        (Option(dir.listFiles) match {
          case Some(files) ⇒ files.toList.filterNot { f ⇒
            f.isDirectory() && f.getName() == templateDirectoryName &&
              (f.getParentFile.getCanonicalFile() == input.getCanonicalFile() || f.getParentFile.getCanonicalFile() == baseDirectoryForLang)
          }
          case None ⇒ Nil
        }).sortWith(_.getName < _.getName)
    }
  }
  protected def isMarkdown(f: File) = (
    !f.isDirectory &&
    !f.getName.startsWith(".") &&
    (f.getName.endsWith(".markdown") || f.getName.endsWith(".md")))
  protected def updateProperties(location: BookletStorage.Location, properties: Properties) {
    implicit val implicitProperties = properties
    if (!location.appResourcesPath.exists()) {
      location.appResourcesPath.mkdir()
      writeTemplates(location.appResourcesPath, location.lang)
    } else if (!location.appResourcesPathLang.exists()) {
      location.appResourcesPathLang.mkdir()
      writeTemplates(location.appResourcesPathLang, location.lang)
    }

    // Path with the base directory.
    properties.setProperty("basePath", input.getCanonicalPath())

    // Path to the index markdown file.
    Settings.indexMarkdownLocation = findResource(Settings.indexMarkdown, location.resourcesPath, location.resourcesPathLang) getOrElse {
      throw new IOException(s"Index markdown file '${Settings.indexMarkdown}' not found.")
    }
    // Path to the index template file.
    Settings.indexTemplateLocation = findResource(Settings.indexTemplate, location.resourcesPath, location.resourcesPathLang) getOrElse {
      throw new IOException(s"Template file '${Settings.indexTemplate}' not found.")
    }
    // Path to the templatePageContent file.
    Settings.templatePageContentLocation = findResource(Settings.templatePageContent, location.resourcesPath, location.resourcesPathLang) getOrElse {
      throw new IOException(s"Template file '${Settings.templatePageContent}' not found.")
    }
    // Path to the templatePageDeepContents file.
    Settings.templatePageDeepContentsLocation = findResource(Settings.templatePageDeepContents, location.resourcesPath, location.resourcesPathLang) getOrElse {
      throw new IOException(s"Template file '${Settings.templatePageDeepContents}' not found.")
    }
    // Path to the templatePageScroll file.
    Settings.templatePageScrollLocation = findResource(Settings.templatePageScroll, location.resourcesPath, location.resourcesPathLang) getOrElse {
      throw new IOException(s"Template file '${Settings.templatePageScroll}' not found.")
    }
  }
  protected def findResource(name: String, templatePath: File, templatePathLang: File): Option[File] = {
    val file = new File(templatePath, name)
    val fileForLang = new File(templatePathLang, name)
    if (fileForLang.isFile())
      Some(fileForLang)
    else if (file.isFile())
      Some(file)
    else
      None
  }
}

object BookletStorage {
  def apply(input: File, properties: Properties = new Properties) = new BookletStorage(input, properties)
  /** Instance with booklet directories. */
  class Location(input: File, val lang: String)(implicit properties: Properties) {
    val isDefault = lang == Settings.defaultLanguage
    /** Base directory. */
    val base = input
    /** Temporary directory. */
    val app = Booklet.tmpDirectory
    /** Temporary directory with resources. */
    val appResourcesPath = new File(app, Settings.template)
    /** User directory with resources. */
    val userResourcesPath = new File(input, Settings.template)

    /** Base directory for language. */
    val baseLang = if (isDefault) input else new File(input, lang)
    /** Temporary directory for language. */
    val appLang = if (isDefault) Booklet.tmpDirectory else new File(Booklet.tmpDirectory, lang)
    /** Temporary directory with resources for language. */
    val appResourcesPathLang = new File(appLang, Settings.template)
    /** User directory with resources for language. */
    val userResourcesPathLang = new File(baseLang, Settings.template)

    val (resourcesPath, resourcesPathLang) = if (userResourcesPath.exists())
      (userResourcesPath, userResourcesPathLang)
    else
      (appResourcesPath, appResourcesPathLang)
  }
}
