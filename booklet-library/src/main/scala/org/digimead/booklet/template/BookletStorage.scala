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

import scala.collection.mutable

import org.digimead.booklet.Booklet
import org.digimead.booklet.Resources
import org.digimead.booklet.Settings
import org.digimead.booklet.Version
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Leaf
import org.digimead.booklet.content.Section
import org.digimead.booklet.discounter.BookletDiscounter
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
      Booklet.merge(Booklet.mergeWithFiles(new Properties, baseBookletPropertiesFile), this.properties)
    else
      properties
    updateProperties(new BookletStorage.Location(input, Settings.defaultLanguage)(result), result)
    result
  }
  def baseBookletPropertiesFile(implicit properties: Properties) = new File(input, Settings.templateProperties)
  def globalized = {
    implicit val baseProperties = Booklet.merge(baseBookletProperties, properties)
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
      lang -> Content(location, rootSection(location.baseLang, lang)(properties), files, favicon)
    }
    // compress cache
    BookletStorage.knockoffCache --= BookletStorage.knockoffCache.keys.filterNot(_.exists())
    // result
    Globalized(Map(contents: _*))
  }
  def rootSection(dir: File, lang: String)(implicit properties: Properties): Section = {
    def emptySection = Section(None, "", Seq.empty, Nil)
    if (dir.exists)
      bookletSection(None, "", dir, lang).headOption getOrElse emptySection
    else
      emptySection
  }
  def read(file: File) = scala.io.Source.fromFile(file).mkString("")
  def knock(file: File)(implicit properties: Properties): (Seq[Block], Properties) = BookletStorage.knockoffCache.get(file) match {
    case Some((mtime, cachedBlocks, cachedProperties, markdownProperties)) if mtime == file.lastModified() && properties == cachedProperties ⇒
      (cachedBlocks, markdownProperties)
    case _ ⇒
      val frontin = Frontin(read(file))
      val markdownProperties = Booklet.mergeWithStrings(properties, frontin.header.toSeq: _*)
      try {
        val blocks = BookletDiscounter.knockoff(Printer.process(frontin body)(markdownProperties), markdownProperties)
        BookletStorage.knockoffCache(file) = (file.lastModified(), blocks, properties, markdownProperties)
        ((blocks, markdownProperties))
      } catch {
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

  protected def bookletSection(fileName: Option[String], localPath: String, dir: File, lang: String)(implicit properties: Properties): Option[Section] = {
    val files = getFiles(dir, lang)
    files.find(isMarkdown).map { head ⇒
      val (blocks, markdownProperties) = knock(head)
      val childFiles = files.filterNot { _ == head } filterNot { f ⇒ f.isDirectory && Settings.languages.contains(f.getName) }
      val children = childFiles.flatMap { f ⇒
        if (isMarkdown(f))
          try {
            val (blocks, markdownProperties) = knock(f)
            Seq(Leaf(Some(f.getName), localPath + "/" + f.getName, blocks)(markdownProperties))
          } catch {
            case e: Throwable ⇒
              log.error(s"Markdown file '${f.getName}' is broken: " + e.getMessage(), e)
              Seq.empty
          }
        else {
          Settings.excludeFolder match {
            case Some(filter) ⇒
              if (!filter.r.pattern.matcher(f.getName).matches)
                bookletSection(Some(f.getName), localPath + "/" + f.getName, f, lang).toSeq
              else {
                log.debug(s"Skip folder '${f.getName}' because of filter /$filter/")
                Seq.empty
              }
            case None ⇒
              bookletSection(Some(f.getName), localPath + "/" + f.getName, f, lang).toSeq
          }
        }
      }
      Section(fileName, localPath, blocks, children)(markdownProperties)
    }
  }
  /**
   *  Get files from the directory.
   *  @return (List[markdown files], List[other files])
   */
  protected def getFiles(dir: File, lang: String)(implicit properties: Properties): List[File] = {
    val baseDirectoryForLang = new File(input, lang)
    val templateDirectoryName = Settings.template
    val files = Settings.excludeMarkdown match {
      case Some(filter) ⇒
        val rFilter = filter.r
        Option(dir.listFiles) match {
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
        }
      case None ⇒
        Option(dir.listFiles) match {
          case Some(files) ⇒ files.toList.filterNot { f ⇒
            f.isDirectory() && f.getName() == templateDirectoryName &&
              (f.getParentFile.getCanonicalFile() == input.getCanonicalFile() || f.getParentFile.getCanonicalFile() == baseDirectoryForLang)
          }
          case None ⇒ Nil
        }
    }
    val sorted = if (Settings.fileNameAsVersion)
      files.map { f ⇒
        (f.getName() match {
          case name if isMarkdown(f) ⇒
            new Version(name.substring(0, name.lastIndexOf(".")))
          case name ⇒
            new Version(name)
        }, f)
      }.sortBy(!_._2.isDirectory).sortWith { case (a, b) ⇒ if (Settings.tocReverse) a._1 > b._1 else a._1 < b._1 }
    else
      files.map { f ⇒
        (f.getName() match {
          case name if isMarkdown(f) ⇒
            name.substring(0, name.lastIndexOf("."))
          case name ⇒
            name
        }, f)
      }.sortBy(!_._2.isDirectory).sortWith { case (a, b) ⇒ if (Settings.tocReverse) a._1 > b._1 else a._1 < b._1 }
    sorted.map(_._2)
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
  /** Knockoff cache */
  val knockoffCache = new mutable.HashMap[File, (Long, Seq[Block], Properties, Properties)] with mutable.SynchronizedMap[File, (Long, Seq[Block], Properties, Properties)]

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
    val userResourcesPath = Settings.resources.map(new File(_, Settings.template)) getOrElse new File(input, Settings.template)

    /** Base directory for language. */
    val baseLang = if (isDefault) input else new File(input, lang)
    /** Temporary directory for language. */
    val appLang = if (isDefault) Booklet.tmpDirectory else new File(Booklet.tmpDirectory, lang)
    /** Temporary directory with resources for language. */
    val appResourcesPathLang = new File(appLang, Settings.template)
    /** User directory with resources for language. */
    val userResourcesPathLang = if (isDefault) userResourcesPath else
      Settings.resources.map(r ⇒ new File(new File(r, lang), Settings.template)) getOrElse new File(baseLang, Settings.template)

    val (resourcesPath, resourcesPathLang) = if (userResourcesPath.exists())
      (userResourcesPath, userResourcesPathLang)
    else
      (appResourcesPath, appResourcesPathLang)

    protected val log = LoggerFactory.getLogger(getClass)
    log.debug("Set template container to " + resourcesPath)
    log.debug(s"Set '$lang' template container to " + resourcesPathLang)
  }
}
