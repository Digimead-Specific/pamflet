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

import java.io.OutputStream
import java.net.URI
import java.util.Properties

import org.digimead.booklet.content.Globalized

import unfiltered.request._
import unfiltered.response._

object Preview {
  def apply(globalized: ⇒ Globalized) = {
    def css(lang: String) = Map.empty ++ globalized(lang).css
    def files(lang: String) = Map.empty ++ globalized(lang).files
    def defaultLanguage = globalized.defaultLanguage
    def languages = globalized.languages
    def faviconResponse(lang: String) =
      globalized(lang).favicon map { responseStreamer } getOrElse NotFound
    def cssResponse(lang: String, name: String) =
      CssContent ~> ResponseString(css(lang)(name))
    def fileResponse(lang: String, name: String) =
      responseStreamer(files(lang)(name))
    def pageResponse(lang: String, name: String) =
      Printer(globalized(lang), globalized, None).printNamed(name).map(Html5).getOrElse { NotFound }

    unfiltered.jetty.Http.anylocal.filter(unfiltered.filter.Planify {
      case GET(Path(Seg(lang :: Nil))) if languages.contains(lang) ⇒
        globalized(lang).pages.headOption.map { page ⇒
          Redirect("/" + lang + "/" + Printer.webify(page))
        }.getOrElse { NotFound }
      case GET(Path(Seg(Nil))) ⇒
        globalized(defaultLanguage).pages.headOption.map { page ⇒
          Redirect("/" + Printer.webify(page))
        }.getOrElse { NotFound }
      case GET(Path(Seg(lang :: "favicon.ico" :: Nil))) if languages.contains(lang) && globalized(lang).favicon.isDefined ⇒
        faviconResponse(lang)
      case GET(Path(Seg("favicon.ico" :: Nil))) if globalized(defaultLanguage).favicon.isDefined ⇒
        faviconResponse(defaultLanguage)
      case GET(Path(Seg(lang :: "css" :: name :: Nil))) if languages.contains(lang) && css(lang).contains(name) ⇒
        cssResponse(lang, name)
      case GET(Path(Seg("css" :: name :: Nil))) if css(defaultLanguage).contains(name) ⇒
        cssResponse(defaultLanguage, name)
      case GET(Path(Seg(lang :: "files" :: name :: Nil))) if languages.contains(lang) && files(lang).contains(name) ⇒
        fileResponse(lang, name)
      case GET(Path(Seg("files" :: name :: Nil))) if files(defaultLanguage).contains(name) ⇒
        fileResponse(defaultLanguage, name)
      case GET(Path(Seg(lang :: name :: Nil))) if languages.contains(lang) ⇒
        pageResponse(lang, name)
      case GET(Path(Seg(name :: Nil))) ⇒
        pageResponse(defaultLanguage, name)
    }).resources(Shared.resources)
  }
  def responseStreamer(uri: URI) =
    new ResponseStreamer {
      def stream(os: OutputStream) {
        val is = uri.toURL.openStream
        try {
          val buf = new Array[Byte](1024)
          Iterator.continually(is.read(buf)).takeWhile(_ != -1)
            .foreach(os.write(buf, 0, _))
        } finally {
          is.close
        }
      }
    }
  case class Html5(content: String) extends ComposeResponse(HtmlContent ~> ResponseString(content))
}
