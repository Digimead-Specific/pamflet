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
package org.digimead.booklet

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.StringReader
import java.nio.charset.Charset
import java.util.Enumeration
import java.util.Properties

import scala.collection.JavaConverters._

import org.slf4j.LoggerFactory

/**
 * Booklet singleton with default settings.
 */
object Booklet {
  /** Name of index template. */
  val indexTemplate = "index.scaml"
  /** Name of the file with index content. */
  val indexMarkdown = "index.markdown"
  /** Name of file with booklet properties. */
  val properties = "booklet.properties"
  /** Name of directory with booklet template. */
  val template = "template"
  /** Name of the file with template for PageContent. */
  val templatePageContent = "pageContent.scaml"
  /** Name of the file with template for DeepContents. */
  val templatePageDeepContents = "pageDeepContents.scaml"
  /** Name of the file with template for PageScroll. */
  val templatePageScroll = "pageScroll.scaml"

  val log = LoggerFactory.getLogger(getClass)

  /** Dump properties */
  def dump(properties: Properties, header: String = "Booklet properties (%d):") = synchronized {
    val keys = properties.propertyNames.asInstanceOf[Enumeration[String]].asScala.toSeq
    log.info(header.format(keys.size))
    for (key ← keys.sorted)
      log.info(key + " -> " + properties.getProperty(key))
  }
  /** Return consolidated properties. */
  def mergeWithStrings(base: Properties, append: String*): Properties = {
    val result = new java.util.Properties
    result.putAll(base)
    for (s ← append) {
      val p = new java.util.Properties
      val is = new StringReader(s)
      try p.load(is) finally { try is.close() catch { case _: Throwable ⇒ } }
      result.putAll(p)
    }
    result
  }
  /** Return consolidated properties. */
  def mergeWithFiles(base: Properties, append: File*): Properties = {
    val result = new java.util.Properties
    result.putAll(base)
    for (f ← append) {
      val p = new java.util.Properties
      val is = new InputStreamReader(new FileInputStream(f), Charset.forName("UTF-8"))
      try p.load(is) finally { try is.close() catch { case _: Throwable ⇒ } }
      result.putAll(p)
    }
    result
  }
  /** Return consolidated properties. */
  def merge(base: Properties, append: Properties*): Properties = {
    val result = new java.util.Properties
    for (p ← base +: append) result.putAll(p)
    result
  }
  object Options {
    val optionVerbose = "optionVerbose"
  }
}
