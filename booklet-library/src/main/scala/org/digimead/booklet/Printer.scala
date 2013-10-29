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

import org.digimead.booklet.content.AuthoredPage
import org.digimead.booklet.content.Content
import org.digimead.booklet.content.ContentPage
import org.digimead.booklet.content.DeepContents
import org.digimead.booklet.content.Globalized
import org.digimead.booklet.content.Page
import org.digimead.booklet.content.ScrollPage
import org.digimead.booklet.content.Section
import org.digimead.booklet.discounter.Discounter
import org.digimead.booklet.discounter.Headers
import org.digimead.booklet.template.Template
import org.slf4j.LoggerFactory

case class Printer(contents: Content, globalized: Globalized, manifest: Option[String]) {
  val relativeBase: String = relative(defaultLanguage)
  protected val log = LoggerFactory.getLogger(getClass)
  def arrow(page: Page) = (page.getProperty("booklet.arrow") getOrElse "❧")
  def defaultLanguage = globalized.defaultLanguage
  def relative(lang: String): String =
    if (contents.isDefaultLang) {
      if (lang == defaultLanguage) ""
      else lang + "/"
    } else {
      if (lang == defaultLanguage) "../"
      else "../" + lang + "/"
    }

  def toc(current: Page) = {
    val href: Page ⇒ String = current match {
      case ScrollPage(_, _) ⇒ (p: Page) ⇒ Headers.BlockNames.fragment(p.name)
      case _ ⇒ Printer.webify
    }

    val link: Page ⇒ xml.NodeSeq = {
      case `current` ⇒
        <div class="current">{ current.name }</div>
      case page ⇒
        {
          <div><a href={ href(page) }>{
            page.name
          }</a></div>
        } ++ ((page, current) match {
          case (page: ContentPage, c: DeepContents) ⇒
            Outline(page)
          case _ ⇒ Nil
        })
    }
    def draw: Page ⇒ xml.NodeSeq = {
      case sect @ Section(_, blocks, children, _) ⇒
        link(sect) ++ list(children)
      case page ⇒ link(page)
    }
    def list(pages: Seq[Page]) = {
      <ol class="toc"> {
        pages.map {
          case page: ContentPage ⇒ <li>{ draw(page) }</li>
          case page ⇒ <li class="generated">{ draw(page) }</li>
        }
      } </ol>
    }
    def display: String = current match {
      case DeepContents(_) | ScrollPage(_, _) ⇒ "show"
      case _ ⇒
        current.getProperty("toc") match {
          case Some("hide") ⇒ "hide"
          case Some("collapse") ⇒ "collap"
          case _ ⇒ "show"
        }
    }
    if (display == "hide") Nil
    else <div class={ "tocwrapper " + display }>
           <a class="tochead nav" style="display: none" href="#toc">❦</a>
           <a name="toc"></a>
           <h4 class="toctitle">Contents</h4>
           <div class="tocbody">
             {
               link(contents.booklet) ++
                 list(current match {
                   case ScrollPage(_, _) ⇒ contents.booklet.children.collect {
                     case cp: ContentPage ⇒ cp
                   }
                   case _ ⇒ contents.booklet.children
                 })
             }
           </div>
         </div>
  }
  def elementComment(page: Page) = page.getProperty("disqus") map { disqusName ⇒
    val disqusCode = """
        var disqus_shortname = '""" + disqusName + """';
        (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        })();
"""

    { <div id="disqus_thread"></div> } ++
      { <script type="text/javascript">{ disqusCode }</script> }
  } getOrElse Nil
  def elementCSS(page: Page): Seq[xml.NodeSeq] = (globalized.defaultContents.css.map {
    case (filename, contents) ⇒
      <link rel="stylesheet" href={ relativeBase + "css/" + filename } type="text/css" media="screen, projection"/>
  }) ++
    (if (contents.isDefaultLang) Nil
    else contents.css.map {
      case (filename, contents) ⇒
        <link rel="stylesheet" href={ "css/" + filename } type="text/css" media="screen, projection"/>
    })
  def elementFavicon(page: Page): Seq[xml.NodeSeq] = contents.favicon match {
    case Some(x) ⇒ <link rel="shortcut icon" href="favicon.ico"/>
    case None ⇒
      if (contents.isDefaultLang) Nil
      else {
        globalized.defaultContents.favicon match {
          case Some(x) ⇒ <link rel="shortcut icon" href={ relativeBase + "favicon.ico" }/>
          case None ⇒ Nil
        }
      }
  }
  def elementGoogleAnalytics(page: Page): Seq[xml.NodeSeq] = page.getProperty("google-analytics").toList.map { uid: String ⇒
    <script type="text/javascript"><!--
            var _gaq = _gaq || [];
            _gaq.push(['_setAccount', '{xml.Unparsed(uid)}']);
            _gaq.push(['_trackPageview']);
            (function() {{
              var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
              ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
              var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
            }})();
            --></script>
  }
  def elementPageNextNav(page: Page, next: Option[Page]): Seq[xml.NodeSeq] = next.map { n ⇒
    <a class="page next nav" href={ Printer.webify(n) }>
      <span class="space">&nbsp;</span>
      <span>{ arrow(page) }</span>
    </a>
  }.toSeq
  def elementPagePrevNav(page: Page, prev: Option[Page]): Seq[xml.NodeSeq] = prev.map { p ⇒
    <a class="page prev nav" href={ Printer.webify(p) }>
      <span class="space">&nbsp;</span>
      <span class="flip">{ arrow(page) }</span>
    </a>
  }.toSeq
  def elementPrettify(page: Page): Seq[xml.NodeSeq] = page.referencedLangs.find { _ ⇒ true }.map { _ ⇒
    {
      <script type="text/javascript" src={ relativeBase + "js/prettify/prettify.js" }></script>
    } ++
      page.prettifyLangs.map { br ⇒
        <script type="text/javascript" src={
          relativeBase + "js/prettify/lang-%s.js".format(br)
        }></script>
      } ++
      <link type="text/css" rel="stylesheet" href={ relativeBase + "css/prettify.css" }/>
      <script type="text/javascript"><!--
        window.onload=function() { prettyPrint(); };
      --></script>
  }.toSeq
  def elementTwitter(page: Page): Seq[xml.NodeSeq] = page.getProperty("twitter").map { twt ⇒
    <script type="text/javascript">
      Booklet.twitter = '{ xml.Unparsed(twt) }
      ';
    </script>
  }.toSeq
  def languageBar(page: Page) =
    if (page.template.languages(page.properties).size < 2) Nil
    else {
      def languageName(langCode: String): String =
        page.getProperty("lang-" + langCode) getOrElse {
          Language.languageName(langCode) getOrElse langCode
        }
      <ul class="language-bar">
        {
          val lis =
            for {
              lang ← page.template.languages(page.properties)
              p ← globalized(lang).pages.find { _.localPath == page.localPath }
            } yield <li><a href={ relative(lang) + Printer.webify(p) }><span class={ "lang-item lang-" + lang }>{ languageName(lang) }</span></a></li>
          if (lis.size < 2) Nil
          else lis
        }
      </ul>
    }

  def print(page: Page) = {
    log.info(s"Print '${page.name}' page.")
    def lastnext(in: List[Page], last: Option[Page]): (Option[Page], Option[Page]) =
      (in, last) match {
        case (List(l, `page`, n, _*), _) ⇒ (Some(l), Some(n))
        case (List(l, `page`), _) ⇒ (Some(l), None)
        case (List(`page`, n, _*), _) ⇒ (last, Some(n))
        case ((_ :: tail), _) ⇒ lastnext(tail, last)
        case _ ⇒ (None, None)
      }
    val (prev, next) = lastnext(contents.pages, None)
    val bigScreen = "screen and (min-device-width: 800px), projection"

    val arrow = page.getProperty("booklet.arrow") getOrElse "❧"
    val colorScheme = page.getProperty("color_scheme") map { "color_scheme-" + _ } getOrElse "color_scheme-redmond"

    val properties = Map(
      "elementComment" -> elementComment(page),
      "elementCSS" -> elementCSS(page),
      "elementFavicon" -> elementFavicon(page),
      "elementGoogleAnalytics" -> elementGoogleAnalytics(page),
      "elementPageNextNav" -> elementPageNextNav(page, next),
      "elementPagePrevNav" -> elementPagePrevNav(page, prev),
      "elementPrettify" -> elementPrettify(page),
      "elementTwitter" -> elementTwitter(page),
      "title" -> "%s — %s".format(contents.title, page.name)) ++
      mapAsScalaMap(page.properties.asInstanceOf[java.util.Map[String, String]])
    if (page.properties.containsKey(Booklet.Options.optionVerbose)) {
      log.info(s"Booklet properties (${properties.size}) for '${page.name}' page:")
      for ((k, v) ← properties.toSeq.sortBy(_._1))
        log.info(k + " -> " + v)
    }
    page.getProperty("layout") match {
      case None ⇒
        val tree = <html>
                     <head>
                       <title>{ properties("title") }</title>
                       { properties("elementFavicon") }
                       <link rel="stylesheet" href={ relativeBase + "css/blueprint/screen.css" } type="text/css" media="screen, projection"/>
                       <link rel="stylesheet" href={ relativeBase + "css/blueprint/grid.css" } type="text/css" media={ bigScreen }/>
                       <link rel="stylesheet" href={ relativeBase + "css/blueprint/print.css" } type="text/css" media="print"/>
                       <!--[if lt IE 8]>
          <link rel="stylesheet" href={ relativeBase + "css/blueprint/ie.css" } type="text/css" media="screen, projection"/>
        <![endif]-->
                       <link rel="stylesheet" href={ relativeBase + "css/booklet.css" } type="text/css" media="screen, projection"/>
                       <link rel="stylesheet" href={ relativeBase + "css/booklet-print.css" } type="text/css" media="print"/>
                       <link rel="stylesheet" href={ relativeBase + "css/booklet-grid.css" } type="text/css" media={ bigScreen }/>
                       <link rel="stylesheet" href={ relativeBase + "css/color_scheme-redmond.css" } type="text/css" media="screen, projection"/>
                       <link rel="stylesheet" href={ relativeBase + "css/color_scheme-github.css" } type="text/css" media="screen, projection"/>
                       <link rel="stylesheet" href={ relativeBase + "css/color_scheme-monokai.css" } type="text/css" media="screen, projection"/>
                       <script type="text/javascript" src={ relativeBase + "js/jquery-1.6.2.min.js" }></script>
                       <script type="text/javascript" src={ relativeBase + "js/jquery.collapse.js" }></script>
                       <script type="text/javascript" src={ relativeBase + "js/booklet.js" }></script>
                       <script type="text/javascript">
                         Booklet.page.language = '{ xml.Unparsed(contents.language) }
                         ';
                       </script>
                       { properties("elementPrettify") }
                       { properties("elementCSS") }
                       <meta charset="utf-8"/>
                       <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                       { properties("elementGoogleAnalytics") }
                       { properties("elementTwitter") }
                     </head>
                     <body class={ colorScheme }>
                       { properties("elementPagePrevNav") }
                       { properties("elementPageNextNav") }
                       <div class="container">
                         <div class="span-16 prepend-1 append-1">
                           <div class="span-16 top nav">
                             <div class="span-16 title">
                               <span>{ contents.title }</span>{
                                 if (contents.title != page.name)
                                   "— " + page.name
                                 else ""
                               }
                             </div>
                           </div>
                         </div>
                         <div class="span-16 prepend-1 append-1 contents">
                           {
                             page match {
                               case page: DeepContents ⇒
                                 toc(page)
                               case page: ContentPage ⇒
                                 Discounter.toXHTML(page.blocks) ++ next.collect {
                                   case n: AuthoredPage ⇒
                                     <div class="bottom nav span-16">
                                       <em>Next Page</em>
                                       <span class="arrow">{ arrow }</span>
                                       <a href={ Printer.webify(n) }> { n.name } </a>
                                       { languageBar(page) }
                                     </div>
                                   case _ ⇒
                                     <div class="bottom nav end span-16">
                                       { languageBar(page) }
                                     </div>
                                 } ++ toc(page) //++ properties("elementComment")
                               case page: ScrollPage ⇒
                                 toc(page) ++ Discounter.toXHTML(page.blocks)
                             }
                           }
                         </div>
                       </div>
                       {
                         page.getProperty("github").map { repo ⇒
                           <a href={ "http://github.com/" + repo } class="fork nav"><img src={ relativeBase + "img/fork.png" } alt="Fork me on GitHub"/></a>
                         }.toSeq
                       }
                       {
                         page.getProperty("twitter").map { twt ⇒
                           <div class="highlight-outer">
                             <div class="highlight-menu">
                               <ul>
                                 <li><button id="highlight-button-twitter"><img src={ relativeBase + "img/twitter-bird-dark-bgs.png" }/></button></li>
                               </ul>
                             </div>
                           </div>
                         }.toSeq
                       }
                     </body>
                   </html>
        val content = manifest.map { mf ⇒
          tree % new scala.xml.UnprefixedAttribute("manifest", mf, scala.xml.Null)
        } getOrElse { tree } map { nodes ⇒
          val html = nodes.head match {
            case <html>{ _* }</html> ⇒ nodes.head
            case _ ⇒ <html>{ nodes }</html>
          }
          val w = new java.io.StringWriter()
          xml.XML.write(w, html, "UTF-8", xmlDecl = false, doctype = xml.dtd.DocType("html", xml.dtd.SystemID("about:legacy-compat"), Nil))
          w.toString()
        }
        content.headOption.getOrElse { throw new IllegalStateException("Unable to convert XML page representation to string.") }
      case Some(layout) ⇒
        val base = new File(page.getProperty("base") getOrElse { throw new IllegalStateException("Unable to find 'base' property.") })
        Template.engine.layout(new File(base, layout).toString())
    }
  }

  def named(name: String) =
    contents.pages.find { page ⇒
      Printer.webify(page) == name
    }

  def printNamed(name: String) = named(name).map(print)
}

object Printer {
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

