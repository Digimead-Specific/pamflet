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

import org.digimead.booklet.Frontin
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Leaf
import org.digimead.booklet.content.Section
import org.digimead.booklet.discounter.Discounter
import org.digimead.booklet.template.Template

import com.tristanhunt.knockoff.Block

class FileStorage(base: File, blocks: Seq[String] = Nil) extends Storage {
  /**
   * Return booklet template.
   * If there is no template then storage consider as plain markdown index.
   */
  def baseBookletProperties: Properties = {
    val properties = new java.util.Properties
    val propertiesFile = new File(base, Storage.bookletTemplate)
    if (propertiesFile.exists() && propertiesFile.isFile() && propertiesFile.canRead())
      mergeWithFiles(properties, propertiesFile)
    else
      properties
  }
  def globalized = {
    implicit val baseProperties = mergeWithStrings(baseBookletProperties, blocks: _*)
    val template = Template()
    val contents = template.languages map { lang ⇒
      val isDefaultLang = lang == template.defaultLanguage
      val dir = if (isDefaultLang) base else new File(base, lang)
      if (!dir.isDirectory())
        throw new IOException("Unable to read " + dir.getCanonicalPath())
      val css = dir.listFiles.filter { _.getName.endsWith(".css") }.map { f ⇒ (f.getName, read(f)) }
      val files = dir.listFiles.filter(_.getName == "files").flatMap(_.listFiles.map { f ⇒ (f.getName, f.toURI) })
      val favicon = dir.listFiles.filter(_.getName == "favicon.ico").headOption.map { _.toURI }
      val properties = if (isDefaultLang) baseProperties else new File(dir, Storage.bookletTemplate) match {
        case file if file.exists() && file.isFile() && file.canRead() ⇒ mergeWithStrings(mergeWithFiles(baseBookletProperties, file), blocks: _*)
        case _ ⇒ baseProperties
      }
      lang -> Content(lang, isDefaultLang, rootSection(template, dir), css, files, favicon, template)
    }
    Globalized(Map(contents: _*), template)
  }
  def rootSection(template: Template, dir: File)(implicit properties: Properties): Section = {
    def emptySection = Section("", Seq.empty, Nil, template)
    if (dir.exists)
      section(template, "", dir).headOption getOrElse emptySection
    else
      emptySection
  }
  def section(template: Template, localPath: String, dir: File)(implicit properties: Properties): Seq[Section] = {
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
          Seq(Leaf(localPath + "/" + f.getName, (knock(template, f), template)))
        else section(template, localPath + "/" + f.getName, f)
      }
      Section(localPath, blocks, children, template)
    }.toSeq
  }
  def read(file: File) = scala.io.Source.fromFile(file).mkString("")
  def knock(template: Template, file: File)(implicit properties: Properties): Seq[Block] = {
    val frontin = Frontin(read(file))
    try Discounter.knockoff(template(frontin body)(mergeWithStrings(properties, frontin.header.toSeq: _*)))
    catch {
      case e: Throwable ⇒
        Console.err.println("Error while processing " + file.toString)
        throw e
    }
  }
  protected def isMarkdown(f: File) = (
    !f.isDirectory &&
    !f.getName.startsWith(".") &&
    (f.getName.endsWith(".markdown") || f.getName.endsWith(".md")))
}

object FileStorage {
  def apply(base: File, blocks: Seq[String] = Nil) = new FileStorage(base, blocks)
}
