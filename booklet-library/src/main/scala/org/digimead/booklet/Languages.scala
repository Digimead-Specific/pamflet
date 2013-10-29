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

import collection.immutable.Map

object Language {
  // see http://en.wikipedia.org/wiki/IETF_language_tag
  val languageNames: Map[String, String] = Map(
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