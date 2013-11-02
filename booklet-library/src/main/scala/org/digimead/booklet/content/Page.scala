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

package org.digimead.booklet.content

import java.util.Properties

import org.digimead.booklet.template.Printer
import org.digimead.booklet.discounter.Headers
import org.digimead.booklet.template.Outline

trait Page {
  def localPath: String
  def name: String
  def prettifyLangs: Set[String]
  def properties: Properties
  def referencedLangs: Set[String]

  /** Get property for given key if present. */
  def getProperty(key: String) = Option(properties.get(key)) map { _.toString }
  /** Set property for given key if present. */
  def setProperty(key: String, value: String) = properties.setProperty(key, value)
}

object Page {
  /** Get visibility class of the table of contents. */
  def tocVisibilityClass(page: Page): String = page match {
    case DeepContents() | ScrollPage(_) ⇒
      "show"
    case _ ⇒
      page.getProperty("toc") match {
        case Some("hide") ⇒ "hide"
        case Some("collapse") ⇒ "collap"
        case _ ⇒ "show"
      }
  }
  /** Get table of contents list. */
  def tocList(page: Page, pages: Seq[Page]): xml.NodeSeq =
    <ol class="toc">{
      pages.map {
        case link: ContentPage ⇒ <li>{ tocLine(page, link) }</li>
        case link ⇒ <li class="generated">{ tocLine(page, link) }</li>
      }
    } </ol>
  /** Get href link for table of contents. */
  def tocHRef(page: Page, link: Page): String = page match {
    case ScrollPage(_) ⇒ Headers.BlockNames.fragment(link.name)
    case _ ⇒ Printer.webify(link)
  }
  /** Get line for table of contents. */
  def tocLine(page: Page, link: Page): xml.NodeSeq = link match {
    case link @ Section(_, blocks, children) ⇒ tocLink(page, link) ++ tocList(page, children)
    case link ⇒ tocLink(page, link)
  }
  def tocLink(page: Page, link: Page): xml.NodeSeq =
    if (link == page)
      <div class="current">{ page.name }</div>
    else
      <div><a href={ tocHRef(page, link) }>{ link.name }</a></div> ++ {
        (link, page) match {
          case (page: ContentPage, c: DeepContents) ⇒ Outline(page)
          case _ ⇒ Nil
        }
      }
}
