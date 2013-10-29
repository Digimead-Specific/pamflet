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

package org.digimead.booklet.storage

import java.io.File
import java.io.IOException
import java.util.Properties

import org.digimead.booklet.Booklet
import org.digimead.booklet.Frontin
import org.digimead.booklet.Shared
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Leaf
import org.digimead.booklet.content.Section
import org.digimead.booklet.discounter.Discounter
import org.digimead.booklet.template.Template
import org.slf4j.LoggerFactory

import com.tristanhunt.knockoff.Block

class FileStorage(val base: File, val properties: Properties = new Properties) extends Storage {
  val template = Template()
  val baseBookletPropertiesFile = new File(base, Settings.properties)
  protected val log = LoggerFactory.getLogger(getClass)

  if (log.isDebugEnabled())
    Booklet.dump(properties, "User properties(%d):")

  /**
   * Return booklet template.
   * If there is no template then storage consider as plain markdown index.
   */
  def baseBookletProperties: Properties = {
    val properties = new Properties
    implicit val result = if (baseBookletPropertiesFile.exists() &&
      baseBookletPropertiesFile.isFile() && baseBookletPropertiesFile.canRead())
      Booklet.mergeWithFiles(properties, baseBookletPropertiesFile)
    else
      properties
    updateProperties(properties, template.defaultLanguage)
    result
  }
  def globalized = {
    implicit val baseProperties = Booklet.merge(baseBookletProperties, properties)
    if (log.isDebugEnabled())
      Booklet.dump(baseProperties, "Base properties(%d):")

    val contents = if (baseBookletPropertiesFile.exists()) {
      log.debug("Process storage as booklets.")
      template.languages map { lang ⇒
        val isDefaultLang = lang == template.defaultLanguage
        val dir = if (isDefaultLang) base else new File(base, lang)
        if (!dir.isDirectory())
          throw new IOException("Unable to read " + dir.getCanonicalPath())
        val css = dir.listFiles.filter { _.getName.endsWith(".css") }.map { f ⇒ (f.getName, read(f)) }
        val files = dir.listFiles.filter(_.getName == "files").flatMap(_.listFiles.map { f ⇒ (f.getName, f.toURI) })
        val favicon = dir.listFiles.filter(_.getName == "favicon.ico").headOption.map { _.toURI }
        val properties = if (isDefaultLang) baseProperties else new File(dir, Settings.properties) match {
          case file if file.exists() && file.isFile() && file.canRead() ⇒ Booklet.merge(Booklet.mergeWithFiles(baseBookletProperties, file), this.properties)
          case _ ⇒ Booklet.merge(new Properties, baseProperties)
        }
        updateProperties(properties, template.defaultLanguage)
        lang -> Content(lang, isDefaultLang, rootSection(template, dir)(properties), css, files, favicon, template)
      }
    } else {
      log.debug("Process storage as index of raw markdowns.")
      template.languages map { lang ⇒
        val isDefaultLang = lang == template.defaultLanguage
        val dir = if (isDefaultLang) base else new File(base, lang)
        val css = dir.listFiles.filter { _.getName.endsWith(".css") }.map { f ⇒ (f.getName, read(f)) }
        val files = dir.listFiles.filter(_.getName == "files").flatMap(_.listFiles.map { f ⇒ (f.getName, f.toURI) })
        val favicon = dir.listFiles.filter(_.getName == "favicon.ico").headOption.map { _.toURI }
        val properties = if (isDefaultLang) baseProperties else new File(dir, Settings.properties) match {
          case file if file.exists() && file.isFile() && file.canRead() ⇒ Booklet.merge(Booklet.mergeWithFiles(baseBookletProperties, file), this.properties)
          case _ ⇒ Booklet.merge(new Properties, baseProperties)
        }
        lang -> Content(lang, isDefaultLang, indexSection(template, dir)(properties), css, files, favicon, template)
      }
    }
    Globalized(Map(contents: _*), template)
  }
  def indexSection(template: Template, dir: File)(implicit properties: Properties): Section = {
    def emptySection = Section("", Seq.empty, Nil, template)
    if (dir.exists)
      indexSection(template, "", dir).headOption getOrElse emptySection
    else
      emptySection
  }
  def rootSection(template: Template, dir: File)(implicit properties: Properties): Section = {
    def emptySection = Section("", Seq.empty, Nil, template)
    if (dir.exists)
      bookletSection(template, "", dir).headOption getOrElse emptySection
    else
      emptySection
  }
  def read(file: File) = scala.io.Source.fromFile(file).mkString("")
  def knock(template: Template, file: File)(implicit properties: Properties): Seq[Block] = {
    val frontin = Frontin(read(file))
    try Discounter.knockoff(template(frontin body)(Booklet.mergeWithStrings(properties, frontin.header.toSeq: _*)))
    catch {
      case e: Throwable ⇒
        Console.err.println("Error while processing " + file.toString)
        throw e
    }
  }
  def writeTemplates(target: File, lang: String)(implicit properties: Properties) {
    log.debug(s"Write templates to ${target.getCanonicalPath()} for '${lang}'.")
    if (lang == template.defaultLanguage)
      Shared.writeTo(Shared.resourcePaths(Set(), true), target)
    else
      Shared.writeTo(Shared.resourcePaths(Set(), true), target, lang)
  }

  protected def bookletSection(template: Template, localPath: String, dir: File)(implicit properties: Properties): Seq[Section] = {
    val files: List[File] = (Option(dir.listFiles) match {
      case None ⇒ Nil
      case Some(files) ⇒ files.toList
    }).sortWith {
      _.getName < _.getName
    }
    files.find(isMarkdown).map { head ⇒
      val blocks = knock(template, head)
      val childFiles = files.filterNot { _ == head } filterNot { f ⇒
        f.isDirectory && template.languages.contains(f.getName)
      }
      val children = childFiles.flatMap { f ⇒
        if (isMarkdown(f))
          try Some(Seq(Leaf(localPath + "/" + f.getName, (knock(template, f), template))))
          catch {
            case e: Throwable ⇒
              log.error(s"Markdown file '${f.getName}' is broken: " + e.getMessage(), e)
              None
          }
        else
          Some(bookletSection(template, localPath + "/" + f.getName, f))
      }
      Section(localPath, blocks, children.flatten, template)
    }.toSeq
  }
  protected def indexSection(template: Template, localPath: String, dir: File)(implicit properties: Properties): Seq[Section] = {
    val files: List[File] = (Option(dir.listFiles) match {
      case None ⇒ Nil
      case Some(files) ⇒ files.toList
    }).sortWith {
      _.getName < _.getName
    }
    val blocks = knock(template, new File(properties.getProperty("indexMarkdownFile")))
    files.find(isMarkdown).map { head ⇒
      val childFiles = files.filterNot { f ⇒
        f.isDirectory && template.languages.contains(f.getName)
      }
      val children = childFiles.flatMap { f ⇒
        if (isMarkdown(f))
          try Some(Seq(Leaf(localPath + "/" + f.getName, (knock(template, f), template))))
          catch {
            case e: Throwable ⇒
              log.error(s"Markdown file '${f.getName}' is broken: " + e.getMessage(), e)
              None
          }
        else
          Some(indexSection(template, localPath + "/" + f.getName, f))
      }
      Section(localPath, blocks, children.flatten, template)
    }.toSeq
  }
  protected def isMarkdown(f: File) = (
    !f.isDirectory &&
    !f.getName.startsWith(".") &&
    (f.getName.endsWith(".markdown") || f.getName.endsWith(".md")))
  protected def updateProperties(properties: Properties, lang: String) {
    implicit val bookletProperties = properties
    val isDefaultLang = lang == template.defaultLanguage
    val baseLang = new File(base, lang)
    val appLang = new File(Template.tmpDirectory, lang)
    val appTemplatePath = new File(Template.tmpDirectory, Settings.template)
    val appTemplatePathLang = if (isDefaultLang) appTemplatePath else new File(appLang, Settings.template)
    val userTemplatePath = new File(base, Settings.template)
    val userTemplatePathLang = new File(baseLang, Settings.template)
    val templateName = Option(properties.getProperty("templateName")) getOrElse {
      if (baseBookletPropertiesFile.exists()) Booklet.pageTemplate else Booklet.indexTemplate
    }

    if (!appTemplatePath.exists()) {
      appTemplatePath.mkdir()
      writeTemplates(appTemplatePath, lang)
    } else if (!appTemplatePathLang.exists()) {
      appTemplatePathLang.mkdir()
      writeTemplates(appTemplatePathLang, lang)
    }

    // Path with the base directory.
    properties.setProperty("basePath", base.getCanonicalPath())

    // Path to the template file.
    val templateFileDefault = new File(appTemplatePath, templateName)
    val templateFileDefaultForLang = new File(appTemplatePathLang, templateName)
    val templateFileCustom = new File(userTemplatePath, templateName)
    val templateFileCustomForLang = if (isDefaultLang) templateFileCustom else new File(userTemplatePathLang, templateName)
    if (templateFileCustomForLang.isFile())
      properties.setProperty("templateFile", templateFileCustomForLang.getCanonicalPath())
    else if (templateFileCustom.isFile())
      properties.setProperty("templateFile", templateFileCustom.getCanonicalPath())
    else if (templateFileDefaultForLang.isFile())
      properties.setProperty("templateFile", templateFileDefaultForLang.getCanonicalPath())
    else if (templateFileDefault.isFile())
      properties.setProperty("templateFile", templateFileDefault.getCanonicalPath())
    else
      throw new IOException(s"Template file '${templateName}' not found.")

    // Path to the index markdown file.
    val indexMarkdownFileDefault = new File(appTemplatePath, Settings.indexMarkdown)
    val indexMarkdownFileDefaultForLang = new File(appTemplatePath, Settings.indexMarkdown)
    val indexMarkdownFileCustom = new File(userTemplatePath, Settings.indexMarkdown)
    val indexMarkdownFileCustomForLang = if (isDefaultLang) indexMarkdownFileCustom else new File(userTemplatePathLang, Settings.indexMarkdown)
    if (indexMarkdownFileCustomForLang.isFile())
      properties.setProperty("indexMarkdownFile", indexMarkdownFileCustomForLang.getCanonicalPath())
    else if (indexMarkdownFileCustom.isFile())
      properties.setProperty("indexMarkdownFile", indexMarkdownFileCustom.getCanonicalPath())
    else if (indexMarkdownFileDefaultForLang.isFile())
      properties.setProperty("indexMarkdownFile", indexMarkdownFileDefaultForLang.getCanonicalPath())
    else if (indexMarkdownFileDefault.isFile())
      properties.setProperty("indexMarkdownFile", indexMarkdownFileDefault.getCanonicalPath())
    else
      throw new IOException(s"Index markdown file '${Settings.indexMarkdown}' not found.")
  }
}

object FileStorage {
  def apply(base: File, properties: Properties = new Properties) = new FileStorage(base, properties)
}
