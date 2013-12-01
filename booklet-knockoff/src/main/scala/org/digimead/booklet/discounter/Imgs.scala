/**
 * Booklet Knockoff - Extensions to the Knockoff Markdown parser.
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

package org.digimead.booklet.discounter

import scala.xml.Node

import com.tristanhunt.knockoff.{ Discounter ⇒ KDiscounter }
import com.tristanhunt.knockoff.Span
import com.tristanhunt.knockoff.XHTMLWriter

object Imgs {
  // see http://www.w3.org/html/wg/drafts/html/master/syntax.html#void-elements
  trait Html5 extends KDiscounter { self: XHTMLWriter ⇒
    override lazy val imageLinkToXHTML: (Seq[Span], String, Option[String]) ⇒ Node = {
      (spans, url, title) ⇒
        <img src={ url } title={ title.getOrElse(null) } alt={ spans.map(spanToXHTML(_)) }/>
    }
  }
}
