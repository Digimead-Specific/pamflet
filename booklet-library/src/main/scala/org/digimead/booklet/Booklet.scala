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

import org.digimead.booklet.content.Content
import org.digimead.booklet.template.Printer
import org.fusesource.scalate.TemplateEngine
import org.slf4j.LoggerFactory

/**
 * Booklet singleton with default settings.
 */
object Booklet {
  val log = LoggerFactory.getLogger(getClass)
  /** Scalate engine. */
  lazy val engine = new TemplateEngine
  /** Temporary directory for booklet. */
  lazy val tmpDirectory = {
    val dir = new File(engine.tmpDirectory, "Booklet-" + System.currentTimeMillis() + System.nanoTime())
    dir.mkdirs()
    dir
  }

  /** Dump properties. */
  def dump(properties: Properties, header: String = "Booklet properties (%d):") = synchronized {
    val keys = properties.propertyNames.asInstanceOf[Enumeration[String]].asScala.toSeq
    log.info(header.format(keys.size))
    for (key ← keys.sorted)
      log.info(key + " -> " + properties.getProperty(key))
  }
  /** Get booklet manifest for offline usage. */
  def manifest(contents: Content): String = {
    val favicon = contents.favicon.toList.map { case u ⇒ ("favicon.ico", u) }
    val files = contents.files.toList.map { case (nm, u) ⇒ ("files/" + nm, u) }
    val paths = Resources.paths(contents.prettifyLangs)(contents.rootSection.properties)

    ("CACHE MANIFEST" ::
      // cache file must change between updates
      ("# " + new java.util.Date) ::
      contents.pages.map { p ⇒ Printer.webify(p) } :::
      files.map { case (n, _) ⇒ n } :::
      favicon.map { case (n, _) ⇒ n } :::
      paths).mkString("\n")
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
}
