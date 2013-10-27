/**
 * Pamflet - a publishing library for short texts.
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

package org.digimead.pamflet

import java.io.{ File ⇒ JFile }

import scala.collection.immutable.Map

import org.digimead.pamflet.content.Content
import org.digimead.pamflet.content.Globalized
import org.digimead.pamflet.content.Leaf
import org.digimead.pamflet.content.Section
import org.digimead.pamflet.discounter.Discounter

import com.tristanhunt.knockoff._

trait Storage {
  def globalized: Globalized
}

object Storage {
  case class File(base: JFile) extends Storage {
    def propFile(dir: JFile): Option[JFile] =
      new JFile(dir, "template.properties") match {
        case file if file.exists ⇒ Some(file)
        case _ ⇒ None
      }
    def globalized = {
      val contents = Map(defaultTemplate.languages map { lang ⇒
        val isDefaultLang = lang == defaultTemplate.defaultLanguage
        val dir = if (isDefaultLang) base
        else new JFile(base, lang)
        val css = dir.listFiles.filter {
          _.getName.endsWith(".css")
        }.map { f ⇒ (f.getName, read(f)) }
        val files = dir.listFiles.filter(_.getName == "files").
          flatMap(_.listFiles.map { f ⇒ (f.getName, f.toURI) })
        val favicon = dir.listFiles.filter(_.getName == "favicon.ico").headOption.
          map { _.toURI }
        val propFiles = if (isDefaultLang) propFile(base).toSeq
        else propFile(base).toSeq ++ propFile(dir).toSeq
        lang -> Content(lang, isDefaultLang, rootSection(dir, propFiles), css, files, favicon, defaultTemplate)
      }: _*)
      Globalized(contents, defaultTemplate)
    }
    def rootSection(dir: JFile, propFiles: Seq[JFile]): Section = {
      def emptySection = Section("", Seq.empty, Nil, defaultTemplate)
      if (dir.exists) section("", dir, propFiles).headOption getOrElse emptySection
      else emptySection
    }
    def section(localPath: String, dir: JFile, propFiles: Seq[JFile]): Seq[Section] = {
      val files: List[JFile] = (Option(dir.listFiles) match {
        case None ⇒ Nil
        case Some(files) ⇒ files.toList
      }).sortWith {
        _.getName < _.getName
      }
      files.find(isMarkdown).map { head ⇒
        val (blocks, template) = knock(head, propFiles)
        val childFiles = files.filterNot { _ == head } filterNot { f ⇒
          f.isDirectory && defaultTemplate.languages.contains(f.getName)
        }
        val children = childFiles.flatMap { f ⇒
          if (isMarkdown(f))
            Seq(Leaf(localPath + "/" + f.getName, knock(f, propFiles)))
          else section(localPath + "/" + f.getName, f, propFiles)
        }
        Section(localPath, blocks, children, template)
      }.toSeq
    }
    def read(file: JFile) = scala.io.Source.fromFile(file).mkString("")
    def knock(file: JFile, propFiles: Seq[JFile]): (Seq[Block], Template) = {
      val frontin = Frontin(read(file))
      val template = Template.String(propFiles, frontin header)
      try {
        Discounter.knockoff(template(frontin body)) -> template
      } catch {
        case e: Throwable ⇒
          Console.err.println("Error while processing " + file.toString)
          throw e
      }
    }
    def isMarkdown(f: JFile) = (
      !f.isDirectory &&
      !f.getName.startsWith(".") &&
      (f.getName.endsWith(".markdown") || f.getName.endsWith(".md")))
    def defaultTemplate = Template.String(propFile(base).toSeq, None)
  }
}
