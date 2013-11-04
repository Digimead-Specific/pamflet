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
import java.io.OutputStream
import java.net.URI

import org.digimead.booklet.Booklet
import org.digimead.booklet.Settings
import org.slf4j.LoggerFactory

import unfiltered.request._
import unfiltered.response._

object Preview {
  lazy val log = LoggerFactory.getLogger(getClass)

  def apply(storage: Storage) = {
    def files(lang: String) = Map.empty ++ storage.globalized(lang).files
    def fileContent(file: File): String = {
      val source = io.Source.fromFile(file, "utf-8")
      val content = source.mkString
      source.close
      content
    }
    def defaultLanguage = storage.globalized.language
    def languages = storage.globalized.languages
    def faviconResponse(lang: String) =
      storage.globalized(lang).favicon map { responseStreamer } getOrElse NotFound
    def fileResponse(lang: String, name: String) =
      responseStreamer(files(lang)(name))
    def pageResponse(lang: String, name: String) = {
      implicit val properties = storage.globalized.properties
      val result = if (name == Settings.manifest)
        Some(Booklet.manifest(storage.globalized(lang)))
      else
        Printer(storage.globalized(lang), storage.globalized).printNamed(name)
      result.map(Html5).getOrElse { NotFound ~> ResponseString(s"File ${name}($lang) not found.") }
    }

    log.info("Warm up Scalate engine.")
    storage.globalized(defaultLanguage).pages.headOption.map { page ⇒ pageResponse(defaultLanguage, page.name) }

    unfiltered.jetty.Http.anylocal.filter(unfiltered.filter.Planify {
      case GET(Path(Seg(lang :: Nil))) if languages.contains(lang) ⇒
        storage.globalized(lang).pages.headOption.map { page ⇒
          Redirect("/" + lang + "/" + Printer.webify(page))
        }.getOrElse { NotFound }
      case GET(Path(Seg(Nil))) ⇒
        storage.globalized(defaultLanguage).pages.headOption.map { page ⇒
          Redirect("/" + Printer.webify(page))
        }.getOrElse { NotFound }
      case GET(Path(Seg(lang :: "favicon.ico" :: Nil))) if languages.contains(lang) && storage.globalized(lang).favicon.isDefined ⇒
        faviconResponse(lang)
      case GET(Path(Seg("favicon.ico" :: Nil))) if storage.globalized(defaultLanguage).favicon.isDefined ⇒
        faviconResponse(defaultLanguage)
      case GET(Path(Seg(lang :: "css" :: name))) if languages.contains(lang) ⇒
        val content = storage.globalized(lang)
        val resource = new File(content.location.resourcesPathLang, ("css" +: name).mkString(File.separator))
        if (resource.isFile() && resource.canRead())
          CssContent ~> ResponseString(fileContent(resource))
        else
          NotFound ~> ResponseString(s"File ${resource} not found.")
      case GET(Path(Seg("css" :: name))) ⇒
        val content = storage.globalized.content
        val resource = new File(content.location.resourcesPathLang, ("css" +: name).mkString(File.separator))
        if (resource.isFile() && resource.canRead())
          CssContent ~> ResponseString(fileContent(resource))
        else
          NotFound ~> ResponseString(s"File ${resource} not found.")
      case GET(Path(Seg(lang :: "js" :: name))) if languages.contains(lang) ⇒
        val content = storage.globalized(lang)
        val resource = new File(content.location.resourcesPathLang, ("js" +: name).mkString(File.separator))
        if (resource.isFile() && resource.canRead())
          JsContent ~> ResponseString(fileContent(resource))
        else
          NotFound ~> ResponseString(s"File ${resource} not found.")
      case GET(Path(Seg("js" :: name))) ⇒
        val content = storage.globalized.content
        val resource = new File(content.location.resourcesPathLang, ("js" +: name).mkString(File.separator))
        if (resource.isFile() && resource.canRead())
          JsContent ~> ResponseString(fileContent(resource))
        else
          NotFound ~> ResponseString(s"File ${resource} not found.")
      case GET(Path(Seg(lang :: "files" :: name :: Nil))) if languages.contains(lang) && files(lang).contains(name) ⇒
        fileResponse(lang, name)
      case GET(Path(Seg("files" :: name :: Nil))) if files(defaultLanguage).contains(name) ⇒
        fileResponse(defaultLanguage, name)
      case GET(Path(Seg(lang :: "img" :: name))) if languages.contains(lang) ⇒
        val content = storage.globalized(lang)
        val resource = new File(content.location.resourcesPathLang, ("img" +: name).mkString(File.separator))
        if (resource.isFile() && resource.canRead())
          responseStreamer(resource.toURI())
        else
          NotFound ~> ResponseString(s"File ${resource} not found.")
      case GET(Path(Seg("img" :: name))) ⇒
        val content = storage.globalized.content
        val resource = new File(content.location.resourcesPathLang, ("img" +: name).mkString(File.separator))
        if (resource.isFile() && resource.canRead())
          responseStreamer(resource.toURI())
        else
          NotFound ~> ResponseString(s"File ${resource} not found.")
      case GET(Path(Seg(lang :: name :: Nil))) if languages.contains(lang) ⇒
        pageResponse(lang, name)
      case GET(Path(Seg(name :: Nil))) ⇒
        pageResponse(defaultLanguage, name)
      case GET(Path(Seg(name))) ⇒
        val content = storage.globalized.content
        val resource = new File(content.location.resourcesPathLang, ("js" +: name).mkString(File.separator))
        NotFound ~> ResponseString(s"File ${resource} not found.")
    })
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
