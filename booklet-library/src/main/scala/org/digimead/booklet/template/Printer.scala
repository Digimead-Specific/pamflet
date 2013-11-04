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

import java.util.Properties

import scala.collection.JavaConversions._

import org.antlr.stringtemplate.StringTemplate
import org.digimead.booklet.Booklet
import org.digimead.booklet.Settings
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.ContentPage
import org.digimead.booklet.content.DeepContents
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Page
import org.digimead.booklet.content.ScrollPage
import org.digimead.booklet.discounter.Headers
import org.slf4j.LoggerFactory

case class Printer(val content: Content, val globalized: Globalized) {
  protected val log = LoggerFactory.getLogger(getClass)

  def print(page: Page) = {
    implicit val implicitProperties = page.properties
    val templateFile = page match {
      case page: ContentPage ⇒
        Settings.templatePageContentLocation getOrElse
          { throw new IllegalStateException("Unable to find 'templatePageContentLocation' property.") }
      case page: DeepContents ⇒
        Settings.templatePageDeepContentsLocation getOrElse
          { throw new IllegalStateException("Unable to find 'templatePageDeepContentsLocation' property.") }
      case page: ScrollPage ⇒
        Settings.templatePageScrollLocation getOrElse
          { throw new IllegalStateException("Unable to find 'templatePageScrollLocation' property.") }
    }
    log.info(s"Print '${page.name}' with template '${templateFile.getName()}'.")
    def lastnext(in: List[Page], last: Option[Page]): (Option[Page], Option[Page]) =
      (in, last) match {
        case (List(l, `page`, n, _*), _) ⇒ (Some(l), Some(n))
        case (List(l, `page`), _) ⇒ (Some(l), None)
        case (List(`page`, n, _*), _) ⇒ (last, Some(n))
        case ((_ :: tail), _) ⇒ lastnext(tail, last)
        case _ ⇒ (None, None)
      }
    val (prev, next) = lastnext(content.pages, None)
    val templateProperties = Map[String, Any]() ++ mapAsScalaMap(page.properties.asInstanceOf[java.util.Map[String, String]]) ++ Map(
      "arrow" -> (page.getProperty("booklet.arrow") getOrElse "❧"),
      "bigScreen" -> "screen and (min-device-width: 800px), projection",
      "colorScheme" -> (page.getProperty("color_scheme") map { "color_scheme-" + _ } getOrElse "color_scheme-redmond"),
      "content" -> content,
      "globalized" -> globalized,
      "offlineManifest" -> (if (Settings.offline) Some(Settings.manifest) else None),
      "next" -> next,
      "page" -> page,
      "prev" -> prev,
      "relativeBase" -> Printer.relative(globalized.language, content, globalized),
      "title" -> "%s — %s".format(content.title, page.name))

    if (Settings.verbose) {
      log.info(s"Booklet properties (${templateProperties.size}) for '${page.name}' page:")
      for ((k, v) ← templateProperties.toSeq.sortBy(_._1))
        log.info(k + " -> " + v)
    }
    Booklet.engine.layout(templateFile.getCanonicalPath(), templateProperties)
  }
  def named(name: String) = content.pages.find(page ⇒ Printer.webify(page) == name)
  def printNamed(name: String) = named(name).map(print)
}

object Printer {
  /**
   * Get HTML file name.
   * File names shouldn't be url encoded, just space converted.
   */
  def fileify(page: Page) = (page.getProperty("out") getOrElse { page.name + ".html" }).replace(' ', '+')
  /** Replace template values in input stream with bound properties. */
  def process(input: CharSequence)(implicit properties: Properties) = {
    val st = new StringTemplate
    st.setTemplate(input.toString)
    st.setAttributes(properties)
    st.toString
  }
  def relative(lang: String, contents: Content, globalized: Globalized): String =
    if (contents.isDefaultLang) {
      if (lang == globalized.language) "" else lang + "/"
    } else {
      if (lang == globalized.language) "../" else "../" + lang + "/"
    }
  def webify(page: Page) = Headers.BlockNames.encode(page.getProperty("out") getOrElse { page.name + ".html" })
}
