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

import org.digimead.booklet.content.Content
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.storage.Storage

object Produce {
  def apply(globalized: Globalized, target: File) {
    globalized.languages foreach { lang ⇒
      if (lang == globalized.defaultLanguage) apply(globalized.defaultContents, globalized, target)
      else apply(globalized(lang), globalized, new File(target, lang))
    }
  }
  def apply(contents: Content, globalized: Globalized, target: File) {
    val manifest = "booklet.manifest"
    val offlineTarget = new File(target + "/offline/")
    val css = contents.css.map { case (nm, v) ⇒ ("css/" + nm, v) }.toList
    val paths = Shared.resourcePaths(contents.prettifyLangs)
    val files = contents.files.toList.map { case (nm, u) ⇒ ("files/" + nm, u) }
    val favicon = contents.favicon.toList.map { case u ⇒ ("favicon.ico", u) }

    // generate the pages in target directory and in
    // subdirectory "offline" with html5 manifest
    List(Some(manifest), None).foreach { manifestOpt ⇒
      val offline = !manifestOpt.isEmpty
      val targetDir = (if (offline) offlineTarget else target)
      val printer = Printer(contents, globalized, manifestOpt)
      contents.pages.foreach { page ⇒
        val pagePath = Printer.fileify(page)
        Storage.writeString(pagePath, printer.print(page), targetDir)
      }
      css.foreach { case (path, contents) ⇒ Storage.writeString(path, contents, targetDir) }
      paths.foreach(path ⇒ Storage.write(path, targetDir, new java.net.URL(Shared.resources, path).openStream()))
      for ((path, uri) ← files ++ favicon)
        Storage.write(path, targetDir, uri.toURL.openStream)
    }

    Storage.writeString(manifest, (
      "CACHE MANIFEST" ::
      // cache file must change between updates
      ("# " + new java.util.Date) ::
      css.map { case (n, _) ⇒ n } :::
      contents.pages.map { p ⇒ Printer.webify(p) } :::
      files.map { case (n, _) ⇒ n } :::
      favicon.map { case (n, _) ⇒ n } :::
      paths).mkString("\n"),
      offlineTarget)
  }
}
