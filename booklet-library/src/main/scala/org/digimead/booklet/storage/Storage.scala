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

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.Properties

import scala.annotation.tailrec

import org.digimead.booklet.{ Booklet ⇒ Default }
import org.digimead.booklet.content.Globalized

trait Storage {
  /** User provided properties. */
  val properties: Properties
  /** Globalized content. */
  def globalized: Globalized

  /**
   * Storage settings.
   */
  /*object Settings {
    /** Name of the file with index content. */
    val indexMarkdown = Option(Storage.this.properties.getProperty("indexMarkdown")) getOrElse Default.properties.indexMarkdown
    /** Name of the file with template for index. */
    val indexTemplate = Option(Storage.this.properties.getProperty("indexTemplate")) getOrElse Default.indexTemplate
    /** Name of the file with booklet properties. */
    val properties = Option(Storage.this.properties.getProperty("properties")) getOrElse Default.properties
    /** Name of the directory with booklet template. */
    val template = Option(Storage.this.properties.getProperty("template")) getOrElse Default.template
    /** Name of the file with template for PageContent. */
    val templatePageContent = Option(Storage.this.properties.getProperty("templatePageContent")) getOrElse Default.templatePageContent
    /** Name of the file with template for DeepContents. */
    val templatePageDeepContents = Option(Storage.this.properties.getProperty("templatePageDeepContents")) getOrElse Default.templatePageDeepContents
    /** Name of the file with template for PageScroll. */
    val templatePageScroll = Option(Storage.this.properties.getProperty("templatePageScroll")) getOrElse Default.templatePageScroll
  }*/
}

object Storage {
  def copy(r: InputStream, w: OutputStream) {
    @tailrec def doCopy: Unit = {
      val byte = r.read()
      if (byte != -1) {
        w.write(byte)
        doCopy
      }
    }
    doCopy
    w.flush()
  }
  def write(path: String, target: File, r: InputStream) {
    val file = new File(target, path)
    new File(file.getParent).mkdirs()
    val w = new FileOutputStream(file)
    copy(r, w)
    r.close()
    w.close()
  }
  def writeString(path: String, contents: String, target: File) {
    write(path, target, new ByteArrayInputStream(contents.getBytes("UTF-8")))
  }
}
