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
import java.io.StringReader

/**
 * SimpleTemplate
 *
 * @param block string blocks with booklet template values
 */
class SimpleTemplate(blocks: Seq[String] = Nil) extends Template {
  /** Return consolidated template properties. */
  protected def properties = {
    val p = new java.util.Properties
    for (s ← blocks) {
      val q = new java.util.Properties
      q.load(new StringReader(s))
      p.putAll(q)
    }
    p
  }
}

object SimpleTemplate {
  def apply(blocks: Seq[String]) = new SimpleTemplate(blocks)
}
