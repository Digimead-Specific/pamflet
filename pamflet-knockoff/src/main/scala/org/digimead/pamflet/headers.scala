/**
 * Pamflet Knockoff - Extensions to the Knockoff Markdown parser.
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

package org.digimead.pamflet

import scala.xml.Node
import com.tristanhunt.knockoff._
import java.net.URLEncoder

trait IdentifiedHeaders extends Discounter { self: TextWriter =>
  def headerText( spans : Seq[Span] ) : String = {
    val stringWriter = new java.io.StringWriter
    spans.map( self.spanToText(_)(stringWriter) )
    return stringWriter.toString
  }
  override def headerToXHTML = (level, spans) => {
    val name = BlockNames.encode(BlockNames.textOf(spans))
    val spanned = spans.map(spanToXHTML)
    val anchored = spanned ++
      <a href={ "#" + name } class="header-link"><span class="header-link-content">&nbsp;</span></a>
    level match {
      case 1 => <h1 id={name}>{ anchored }</h1>
      case 2 => <h2 id={name}>{ anchored }</h2>
      case 3 => <h3 id={name}>{ anchored }</h3>
      case 4 => <h4 id={name}>{ anchored }</h4>
      case 5 => <h5 id={name}>{ anchored }</h5>
      case 6 => <h6>{ spanned }</h6>
      case _ =>
        <div class={ "header" + level }>{ spanned }</div>
    }
  }
}

object BlockNames {
  /** Do not generate ids for higher levels than this */
  val maxLevel = 5
  def encode(str: String) =
    java.net.URLEncoder.encode(str.trim(), "utf-8")
  def fragment(str: String) = "#" + encode(str)
  def textOf(spans: Seq[Span]) =
    spans.flatMap {
      case t: Text => Seq(t.content)
      case h: HTMLSpan => Seq(h.html)
      case _ => Seq()
    }.mkString("")
  def name(blocks: Seq[Block]) =
    blocks.view.collect {
      case h: Header => textOf(h.spans)
    }.headOption.getOrElse { "Untitled" }
}
