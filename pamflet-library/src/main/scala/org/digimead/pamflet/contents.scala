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

package org.digimead.pamflet

import com.tristanhunt.knockoff._
import java.net.URI
import collection.immutable.Map

case class Globalized(
  contents: Map[String, Contents],
  template: Template
) {
  def apply(lang: String): Contents = contents(lang)
  def defaultLanguage: String = template.defaultLanguage
  def languages: Seq[String] = template.languages
  lazy val defaultContents: Contents = apply(defaultLanguage)
}
case class Contents(
  language: String,
  val isDefaultLang: Boolean,
  rootSection: Section,
  css: Seq[(String,String)],
  files: Seq[(String, URI)],
  favicon: Option[URI],
  template: Template
) {
  def traverse(incoming: List[Page], past: List[Page]): List[Page] =
    incoming match {
      case (head @ Section(_,_,_,_)) :: tail =>
        traverse(head.children ::: tail, head :: past)
      case head :: tail =>
        traverse(tail, head :: past)
      case Nil => past.reverse
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
sealed trait Page {
  def name: String
  def prettifyLangs: Set[String]
  def referencedLangs: Set[String]
  def localPath: String
  def template: Template
}
sealed trait AuthoredPage extends Page {
  def blocks: Seq[Block]
  lazy val referencedLangs =
    (Set.empty[String] /: blocks) {
      case (s, FencedCodeBlock(_, _, Some(lang))) => s + lang
      case (s, _) => s
    }
  lazy val prettifyLangs = referencedLangs.filter { lang =>
    try {
      new java.net.URL(Shared.resources,
                       "js/prettify/lang-%s.js".format(lang)
                     ).openStream().close()
      true
    } catch {
      case _: Throwable => false
    }
  }
}
trait ContentPage extends AuthoredPage {
  lazy val name = BlockNames.name(blocks)
}
case class Leaf(localPath: String,
                blocks: Seq[Block],
                template: Template) extends ContentPage
object Leaf {
  def apply(localPath: String, t: (Seq[Block], Template)): Leaf = Leaf(localPath, t._1, t._2)
}
case class Section(localPath: String,
                   blocks: Seq[Block],
                   children: List[Page],
                   template: Template) extends ContentPage
case class DeepContents(template: Template) extends Page {
  val name = "Contents in Depth"
  val localPath = name
  def prettifyLangs = Set.empty
  def referencedLangs = Set.empty
}
case class ScrollPage(root: Section,
                      template: Template) extends AuthoredPage {
  val name = "Combined Pages"
  val localPath = name
  def flatten(pages: List[Page]): Seq[Block] =
    pages.view.flatMap {
      case Leaf(_, blocks, _) => blocks
      case Section(_, blocks, children, _) =>
        blocks ++: flatten(children)
      case _ => Seq.empty
    }
  def blocks = root.blocks ++: flatten(root.children)
}
