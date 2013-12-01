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

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node
import org.jsoup.nodes.TextNode
import org.jsoup.select.NodeTraversor
import org.jsoup.select.NodeVisitor
import org.slf4j.LoggerFactory

import com.tristanhunt.knockoff.Block
import com.tristanhunt.knockoff.{ Chunk ⇒ KChunk }
import com.tristanhunt.knockoff.{ Discounter ⇒ KDiscounter }
import com.tristanhunt.knockoff.{ HTMLBlock ⇒ KHTMLBlock }
import com.tristanhunt.knockoff.Span

trait HTMLBlock {
  def findStart(startElement: Regex, in: Reader[Char], sb: StringBuilder): Option[(String, StringBuilder, Reader[Char])] = {
    if (!in.atEnd) sb.append(in.first)
    if (in.atEnd || in.first == '\n') return None
    startElement.findFirstMatchIn(sb.toString).foreach { matcher ⇒
      return Some((matcher.group(1), sb, in.rest))
    }
    findStart(startElement, in.rest, sb)
  }

  @tailrec
  final def findEnd(in: Reader[Char], tagName: String, openCount: Int,
    sb: StringBuilder, buf: StringBuilder): Option[(String, Reader[Char])] = {
    if (!in.atEnd) {
      sb.append(in.first)
      buf.append(in.first)
    }
    if (in.atEnd) return None
    var openCountArg = openCount
    var bufArg = buf
    ("(?i)<[ ]*" + tagName + "[ ]*[^>]*>").r.findFirstMatchIn(buf.toString) match {
      case Some(matcher) ⇒
        openCountArg = openCount + 1
        bufArg = new StringBuilder
      case None ⇒
        ("(?i)</[ ]*" + tagName + "[ ]*>").r.findFirstMatchIn(buf.toString) match {
          case Some(matcher) if openCount == 1 ⇒
            return Some((sb.toString, in.rest))
          case Some(matcher) ⇒
            openCountArg = openCount - 1
            bufArg = new StringBuilder
          case None ⇒
        }
    }
    findEnd(in.rest, tagName, openCountArg, sb, bufArg)
  }
}

object HTMLBlock {
  val startElement = """^<[ ]*([a-zA-Z0-9:_]+)[ \t]*[^>]*?(/?+)>""".r
  /**
   * Advanced HTML chunk that process element's content as part of markdown.
   */
  case class Chunk(content: String) extends KChunk {
    protected val log = LoggerFactory.getLogger(getClass)

    def appendNewBlock(list: collection.mutable.ListBuffer[Block],
      remaining: List[(KChunk, Seq[Span], Position)],
      spans: Seq[Span], position: Position,
      discounter: KDiscounter) = try {
      log.debug("Process HTML block as advanced.")
      val doc = Jsoup.parseBodyFragment(content)
      processHtmlElement(list, doc.body(), position, discounter)
    } catch {
      case e: Throwable ⇒
        log.error("Unable to process HTML block as advanced: " + e.getMessage, e)
        list += KHTMLBlock(content, position)
    }
    def processHtmlElement(list: collection.mutable.ListBuffer[Block], element: Element,
      position: Position, discounter: KDiscounter) {
      new NodeTraversor(new NodeVisitor() {
        def head(node: Node, depth: Int) = node match {
          case textNode: TextNode ⇒
            list ++= discounter.knockoff(textNode.toString())
          case element: Element ⇒
            val accum = new java.lang.StringBuilder
            val out = Option(node.ownerDocument().outputSettings()) getOrElse (new Document("")).outputSettings()
            val outerHtmlHeadMethod = element.getClass().getDeclaredMethod("outerHtmlHead", classOf[java.lang.StringBuilder],
              java.lang.Integer.TYPE, classOf[Document.OutputSettings])
            if (!outerHtmlHeadMethod.isAccessible())
              outerHtmlHeadMethod.setAccessible(true)
            outerHtmlHeadMethod.invoke(element, accum, depth: Integer, out)
            list += KHTMLBlock(accum.toString(), position)
        }
        def tail(node: Node, depth: Int) = node match {
          case textNode: TextNode ⇒
          case element: Element ⇒
            val accum = new java.lang.StringBuilder
            val out = Option(node.ownerDocument().outputSettings()) getOrElse (new Document("")).outputSettings()
            val outerHtmlTailMethod = element.getClass().getDeclaredMethod("outerHtmlTail", classOf[java.lang.StringBuilder],
              java.lang.Integer.TYPE, classOf[Document.OutputSettings])
            if (!outerHtmlTailMethod.isAccessible())
              outerHtmlTailMethod.setAccessible(true)
            outerHtmlTailMethod.invoke(element, accum, depth: Integer, out)
            list += KHTMLBlock(accum.toString(), position)
        }
      }).traverse(element)
    }
  }
}
