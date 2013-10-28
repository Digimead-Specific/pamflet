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

import org.digimead.booklet.content.AuthoredPage
import org.digimead.booklet.discounter.Discounter
import org.digimead.booklet.discounter.Headers

import com.tristanhunt.knockoff.Header

object Outline {
  private case class Return(nodes: xml.NodeSeq, rest: Seq[Header])
  def apply(page: AuthoredPage) = {
    def anchor(name: String) =
      <a href={ Printer.webify(page) + Headers.BlockNames.fragment(name) }>{ name }</a>

    def build(blocks: Seq[Header], cur: Int): Return =
      blocks match {
        case Seq(a, b, tail @ _*) if a.level == cur && b.level > cur ⇒
          val nested = build(b +: tail, b.level)
          val after = build(nested.rest, cur)
          val name = Discounter.headerText(a.spans)
          Return((
            <li>
              { anchor(name) }
              <ul class="outline"> { nested.nodes } </ul>
            </li>) ++ after.nodes, after.rest)
        case Seq(a, tail @ _*) if a.level > cur ⇒
          val Return(nodes, rest) = build(blocks, a.level)
          Return(nodes, rest)
        case Seq(a, tail @ _*) if a.level == cur ⇒
          val Return(nodes, rest) = build(tail, cur)
          val name = Discounter.headerText(a.spans)
          Return((<li> { anchor(name) } </li>) ++ nodes, rest)
        case _ ⇒
          Return(Seq.empty, blocks)
      }
    val headers = page.blocks.collect {
      case h: Header if h.level <= Headers.BlockNames.maxLevel ⇒ h
    }
    headers match {
      case Seq(head, elem, rest @ _*) ⇒
        <ul class="outline"> {
          build(elem +: rest, 0).nodes
        } </ul>
      case _ ⇒ Seq.empty
    }
  }
}
