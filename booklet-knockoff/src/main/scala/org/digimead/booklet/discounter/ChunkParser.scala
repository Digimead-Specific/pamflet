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

import java.io.StringReader

import java.util.Properties

import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader

import com.tristanhunt.knockoff.Block
import com.tristanhunt.knockoff.{ Chunk ⇒ KChunk }
import com.tristanhunt.knockoff.{ ChunkParser ⇒ KChunkParser }
import com.tristanhunt.knockoff.{ Discounter ⇒ KDiscounter }
import com.tristanhunt.knockoff.EmptySpace
import com.tristanhunt.knockoff.HTMLChunk
import com.tristanhunt.knockoff.Span
import com.tristanhunt.knockoff.Text
import com.tristanhunt.knockoff.TextChunk

trait ChunkParser extends KChunkParser {
  lazy val brush: Parser[KChunk] = """[ ]*[^\n]+""".r ^^ { b ⇒ TextChunk(b.trim) }
  override lazy val chunk: Parser[KChunk] = {
    pageProperty | htmlBlock | horizontalRule | leadingStrongTextBlock | leadingEmTextBlock | bulletItem |
      numberedItem | indentedChunk | header | blockquote | linkDefinition |
      fencedChunk | textBlockWithBreak | textBlock | emptyLines | emptySpace
  }
  override lazy val htmlBlock = new Parser[KChunk] with HTMLBlock {
    def apply(in: Reader[Char]): ParseResult[KChunk] = findStart(in, new StringBuilder) match {
      case Some((tagName, sb, rest)) ⇒
        findEnd(rest, tagName, 1, sb, new StringBuilder) match {
          case Some((text, rest)) ⇒
            // Settings.htmlAsMarkdown
            if (BookletDiscounter.pageProperties.value.getProperty("htmlAsMarkdown", "N").toUpperCase() == "Y")
              Success(HTMLBlock.Chunk(text), rest)
            else
              Success(HTMLChunk(text), rest)
          case None ⇒
            Failure("No end tag found for " + tagName, in)
        }
      case None ⇒
        Failure("No HTML start tag found", in)
    }
    def findStart(in: Reader[Char], sb: StringBuilder) =
      super.findStart(HTMLBlock.startElement, in, sb)
  }
  lazy val fence: Parser[KChunk] = "```" ^^ { _ ⇒ EmptySpace("") }
  lazy val fencedChunk: Parser[ChunkParser.FencedChunk] =
    fence ~> opt(brush) ~ emptyLine ~
      rep1(unquotedTextLine | emptyLine) <~ fence <~ emptyLine ^^ {
        case (brush ~ _) ~ lines ⇒
          ChunkParser.FencedChunk(foldedString(lines), brush.map { _.content })
      }
  /** Modify page property. */
  lazy val pageProperty: Parser[KChunk] = """\s*""".r ~> """<!--.*-->""".r ^^ { str ⇒
    val from = str.indexOf("<!--")
    val to = str.indexOf("-->", from + 4)
    val p = new Properties
    val is = new StringReader(str.substring(from + 4, to).trim)
    try p.load(is) finally { try is.close() catch { case _: Throwable ⇒ } }
    BookletDiscounter.pageProperties.value.putAll(p)
    EmptySpace(" " * str.length())
  }
  lazy val unquotedTextLine: Parser[KChunk] = """(?!```)[^\n]+\n""".r ^^ { TextChunk(_) }

  private def foldedString(texts: List[KChunk]): String = ("" /: texts)((current, text) ⇒ current + text.content)
}

object ChunkParser {
  case class FencedChunk(val content: String, language: Option[String]) extends KChunk {
    def appendNewBlock(list: collection.mutable.ListBuffer[Block],
      remaining: List[(KChunk, Seq[Span], Position)],
      spans: Seq[Span], position: Position,
      discounter: KDiscounter) {
      list += CodeBlock(Text(content), position, language)
    }
  }
  case class CodeBlock(text: Text, position: Position, language: Option[String]) extends Block
}
