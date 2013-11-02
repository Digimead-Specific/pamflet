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

package org.digimead.booklet.content

import org.digimead.booklet.Resources
import org.digimead.booklet.discounter.Fenced

import com.tristanhunt.knockoff.Block

trait AuthoredPage extends Page {
  def blocks: Seq[Block]
  lazy val referencedLangs =
    (Set.empty[String] /: blocks) {
      case (s, Fenced.CodeBlock(_, _, Some(lang))) ⇒ s + lang
      case (s, _) ⇒ s
    }
  lazy val prettifyLangs = referencedLangs.filter { lang ⇒
    try {
      new java.net.URL(Resources(),
        "js/prettify/lang-%s.js".format(lang)).openStream().close()
      true
    } catch {
      case _: Throwable ⇒ false
    }
  }
}
