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

import org.digimead.booklet.Settings

case class Globalized(val contents: Map[String, Content]) {
  def apply(lang: String): Content = contents(lang)
  /** Default content. */
  def content: Content = contents.find { case (lang, content) ⇒ lang == Settings.defaultLanguage(content.properties) }.
    map(_._2).getOrElse { throw new IllegalStateException("Unable to find default content.") }
  /** Default language. */
  def language: String = content.location.lang
  /** All booklet languages. */
  def languages: Seq[String] = Option(content.properties.get("languages")).map(_.toString()) match {
    case Some(xs) ⇒ xs.split(",").toSeq map { _.trim }
    case None ⇒ Seq(Settings.defaultLanguage(content.properties))
  }
  /** Default properties. */
  def properties: Properties = content.properties
}

