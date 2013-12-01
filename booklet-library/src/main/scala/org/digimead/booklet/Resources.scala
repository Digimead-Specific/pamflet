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
import java.util.Properties

import scala.collection.immutable

import org.digimead.booklet.template.Storage
import org.slf4j.LoggerFactory

/**
 * Booklet resources.
 */
object Resources {
  val log = LoggerFactory.getLogger(getClass)

  def apply() = new java.net.URL(getClass.getResource("/template/marker"), ".")
  def paths(prettifyLangs: Set[String] = Set("apollo", "basic", "clj", "css", "dart", "erlang",
    "go", "hs", "lisp", "llvm", "lua", "matlab", "ml", "mumps", "n", "pascal", "proto",
    "r", "rd", "scala", "sql", "tcl", "tex", "vb", "vhdl", "wiki", "xq", "yaml"), withTemplates: Boolean = false)(implicit properties: Properties) =
    {
      val elements = ("CSS.scaml" :: "Comment.scaml" :: "Favicon.scaml" :: "GoogleAnalytics.scaml" ::
        "LanguageBar.scaml" :: "PageNextNav.scaml" :: "PagePrevNav.scaml" :: "Prettify.scaml" :: "Twitter.scaml" :: Nil) map { "element/" + _ }
      if (withTemplates)
        Settings.templatePageContent :: Settings.templatePageDeepContents ::
          Settings.templatePageScroll :: "default.scaml" :: "toc.scaml" :: elements
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
      "jquery-1.6.2.min.js" :: "jquery.collapse.js" :: "booklet.js" :: Nil
    }.map { "js/" + _ } ::: {
      "prettify.js" :: prettifyLangs.map { l ⇒ "lang-%s.js".format(l) }.toList
    }.map { "js/prettify/" + _ }
  def writeTo(paths: Seq[String], target: File, prefix: String = "") = {
    log.debug("Write resources to " + target.getCanonicalPath())
    val pathPrefix = if (prefix.endsWith("/") || prefix.isEmpty()) prefix else prefix + "/"
    paths.foreach { path ⇒
      try Storage.write(path, target, new java.net.URL(Resources(), pathPrefix + path).openStream())
      catch {
        case e: IOException ⇒ log.debug(s"Skip ${pathPrefix + path}")
      }
    }
  }

  object Language {
    // see http://en.wikipedia.org/wiki/IETF_language_tag
    val languageNames: immutable.Map[String, String] = Map(
      "ar" -> "العربية",
      "bn" -> "বাংলা",
      "ca" -> "Català",
      "cs" -> "Čeština",
      "de" -> "Deutsch",
      "en" -> "English",
      "es" -> "Español",
      "fa" -> "فارسی",
      "fi" -> "Suomi",
      "fr" -> "Français",
      "he" -> "עברית",
      "hi" -> "हिन्दी",
      "hu" -> "Magyar",
      "id" -> "Bahasa Indonesia",
      "it" -> "Italiano",
      "ja" -> "日本語",
      "ko" -> "한국어",
      "nl" -> "Nederlands",
      "no" -> "Norsk (Bokmål)",
      "pl" -> "Polski",
      "pt" -> "Português",
      "ru" -> "Русский",
      "sv" -> "Svenska",
      "tr" -> "Türkçe",
      "vi" -> "Tiếng Việt",
      "uk" -> "Українська",
      "zh" -> "中文")

    def languageName(code: String): Option[String] = languageNames get code
  }
}
