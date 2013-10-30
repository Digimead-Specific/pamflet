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

import scala.collection.JavaConversions._

import org.digimead.booklet.content.Content
import org.digimead.booklet.content.ContentPage
import org.digimead.booklet.content.DeepContents
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Page
import org.digimead.booklet.content.ScrollPage
import org.digimead.booklet.discounter.Headers
import org.digimead.booklet.template.Template
import org.slf4j.LoggerFactory

case class Printer(contents: Content, globalized: Globalized, manifest: Option[String]) {
  protected val log = LoggerFactory.getLogger(getClass)

  def print(page: Page) = {
    val templateFile = page match {
      case page: ContentPage ⇒
        new File(page.getProperty("templatePageContentFile") getOrElse
          { throw new IllegalStateException("Unable to find 'templatePageContentFile' property.") })
      case page: DeepContents ⇒
        new File(page.getProperty("templatePageDeepContentsFile") getOrElse
          { throw new IllegalStateException("Unable to find 'templatePageDeepContentsFile' property.") })
      case page: ScrollPage ⇒
        new File(page.getProperty("templatePageScrollFile") getOrElse
          { throw new IllegalStateException("Unable to find 'templatePageScrollFile' property.") })
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
    val (prev, next) = lastnext(contents.pages, None)
    val properties = Map(
      "arrow" -> (page.getProperty("booklet.arrow") getOrElse "❧"),
      "bigScreen" -> "screen and (min-device-width: 800px), projection",
      "colorScheme" -> (page.getProperty("color_scheme") map { "color_scheme-" + _ } getOrElse "color_scheme-redmond"),
      "contents" -> contents,
      "globalized" -> globalized,
      "next" -> next,
      "page" -> page,
      "prev" -> prev,
      "relativeBase" -> Printer.relative(globalized.defaultLanguage, contents, globalized),
      "title" -> "%s — %s".format(contents.title, page.name)) ++
      mapAsScalaMap(page.properties.asInstanceOf[java.util.Map[String, String]])
    if (page.properties.containsKey(Booklet.Options.optionVerbose)) {
      log.info(s"Booklet properties (${properties.size}) for '${page.name}' page:")
      for ((k, v) ← properties.toSeq.sortBy(_._1))
        log.info(k + " -> " + v)
    }
    Template.engine.layout(templateFile.getCanonicalPath(), properties)
  }
  def named(name: String) = contents.pages.find(page ⇒ Printer.webify(page) == name)
  def printNamed(name: String) = named(name).map(print)
}

object Printer {
  def relative(lang: String, contents: Content, globalized: Globalized): String =
    if (contents.isDefaultLang) {
      if (lang == globalized.defaultLanguage) "" else lang + "/"
    } else {
      if (lang == globalized.defaultLanguage) "../" else "../" + lang + "/"
    }
  def webify(page: Page) =
    Headers.BlockNames.encode(page.getProperty("out") getOrElse {
      page.name + ".html"
    })
  /** File names shouldn't be url encoded, just space converted */
  def fileify(page: Page) =
    (page.getProperty("out") getOrElse {
      page.name + ".html"
    }).replace(' ', '+')
}

