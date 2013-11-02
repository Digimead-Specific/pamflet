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

import java.io.File

import org.digimead.booklet.Booklet
import org.digimead.booklet.Resources
import org.digimead.booklet.Settings
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.Globalized

object Produce {
  def apply(globalized: Globalized, target: File): Unit = globalized.languages foreach { lang ⇒
    if (lang == globalized.language)
      apply(globalized.content, globalized, target)
    else
      apply(globalized(lang), globalized, new File(target, lang))
  }
  def apply(content: Content, globalized: Globalized, target: File) {
    implicit val implicitProperties = globalized.properties
    val css = content.css.map { case (nm, v) ⇒ ("css/" + nm, v) }.toList
    val paths = Resources.paths(content.prettifyLangs)
    val files = content.files.toList.map { case (nm, u) ⇒ ("files/" + nm, u) }
    val favicon = content.favicon.toList.map { case u ⇒ ("favicon.ico", u) }

    val printer = Printer(content, globalized)
    content.pages.foreach { page ⇒
      val pagePath = Printer.fileify(page)
      Storage.writeString(pagePath, printer.print(page), target)
    }
    css.foreach { case (path, contents) ⇒ Storage.writeString(path, contents, target) }
    paths.foreach(path ⇒ Storage.write(path, target, new java.net.URL(Resources(), path).openStream()))
    for ((path, uri) ← files ++ favicon)
      Storage.write(path, target, uri.toURL.openStream)
    if (Settings.offline)
      Storage.writeString(Settings.manifest, Booklet.manifest(content), target)
  }
}
