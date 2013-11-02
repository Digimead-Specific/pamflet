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

case class Frontin(header: Option[String], body: String)

object Frontin {
  val seperator = "---"

  def seperates(str: String): Boolean =
    (str.trim == seperator) && (str startsWith seperator)

  def apply(str: String): Frontin =
    str.linesWithSeparators.toList match {
      case Nil ⇒ Frontin(None, "")
      case x :: xs if seperates(x) ⇒
        xs span { !seperates(_) } match {
          case (h, b) ⇒ Frontin(Some(h.mkString("")),
            if (b isEmpty) "" else b.tail.mkString(""))
        }
      case _ ⇒ Frontin(None, str)
    }
}
