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
import java.io.IOException

import org.digimead.booklet.storage.Storage
import org.slf4j.LoggerFactory

/**
 * Booklet resources.
 */
object Shared {
  val log = LoggerFactory.getLogger(getClass)

  def resources = new java.net.URL(getClass.getResource("/template/marker"), ".")
  def resourcePaths(prettifyLangs: Set[String], withTemplates: Boolean = false) =
    {
      if (withTemplates)
        Booklet.pageTemplate :: Booklet.indexTemplate :: Booklet.indexMarkdown :: Nil
      else
        Nil
    } ++ {
      "fork.png" :: "twitter-bird-dark-bgs.png" :: Nil
    }.map { "img/" + _ } ::: {
      "booklet.css" :: "booklet-grid.css" :: "booklet-print.css" :: "color_scheme-redmond.css" ::
        "color_scheme-github.css" :: "color_scheme-monokai.css" :: "prettify.css" :: Nil
    }.map { "css/" + _ } ::: {
      "screen.css" :: "grid.css" :: "print.css" :: "ie.css" :: Nil
    }.map { "css/blueprint/" + _ } ::: {
      "jquery-1.6.2.min.js" :: "jquery.collapse.js" :: "booklet.js" :: "prettify.js" :: Nil
    }.map { "js/" + _ } ::: {
      prettifyLangs.map { l ⇒ "lang-%s.js".format(l) }.toList
    }.map { "js/prettify/" + _ }
  def writeTo(paths: Seq[String], target: File, prefix: String = "") = {
    log.debug("Write resources to " + target.getCanonicalPath())
    val pathPrefix = if (prefix.endsWith("/") || prefix.isEmpty()) prefix else prefix + "/"
    paths.foreach { path ⇒
      try Storage.write(path, target, new java.net.URL(Shared.resources, pathPrefix + path).openStream())
      catch {
        case e: IOException ⇒ log.debug(s"Skip ${pathPrefix + path}")
      }
    }
  }
}
