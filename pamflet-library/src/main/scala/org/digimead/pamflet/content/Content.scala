/**
 * Pamflet - a publishing library for short texts.
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

package org.digimead.pamflet.content

import java.net.URI

import org.digimead.pamflet.Template

case class Content(
  language: String,
  val isDefaultLang: Boolean,
  rootSection: Section,
  css: Seq[(String, String)],
  files: Seq[(String, URI)],
  favicon: Option[URI],
  template: Template) {
  def traverse(incoming: List[Page], past: List[Page]): List[Page] =
    incoming match {
      case (head @ Section(_, _, _, _)) :: tail ⇒
        traverse(head.children ::: tail, head :: past)
      case head :: tail ⇒
        traverse(tail, head :: past)
      case Nil ⇒ past.reverse
    }
  val pamflet = Section(rootSection.localPath,
    rootSection.blocks,
    rootSection.children :::
      DeepContents(template) ::
      ScrollPage(rootSection, template) ::
      Nil,
    rootSection.template)
  val pages = traverse(pamflet.children, pamflet :: Nil)
  val title = pamflet.name
  val prettifyLangs = (Set.empty[String] /: pages) { _ ++ _.prettifyLangs }
}
