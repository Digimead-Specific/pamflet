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

import scala.util.parsing.input.Position

import com.tristanhunt.knockoff.Block
import com.tristanhunt.knockoff.{ Chunk ⇒ KChunk }
import com.tristanhunt.knockoff.ChunkParser
import com.tristanhunt.knockoff.{ ChunkParser ⇒ KChunkParser }
import com.tristanhunt.knockoff.{ Discounter ⇒ KDiscounter }
import com.tristanhunt.knockoff.EmptySpace
import com.tristanhunt.knockoff.Span
import com.tristanhunt.knockoff.Text
import com.tristanhunt.knockoff.TextChunk

object Fenced {
  trait ChunkParser extends KChunkParser {
    override def chunk: Parser[KChunk] = {
      horizontalRule | leadingStrongTextBlock | leadingEmTextBlock | bulletItem |
        numberedItem | indentedChunk | header | blockquote | linkDefinition |
        htmlBlock | fencedChunk | textBlockWithBreak | textBlock | emptyLines | emptySpace
    }
    def fencedChunk: Parser[Chunk] =
      fence ~> opt(brush) ~ emptyLine ~
        rep1(unquotedTextLine | emptyLine) <~ fence <~ emptyLine ^^ {
          case (brush ~ _) ~ lines ⇒
            Chunk(foldedString(lines), brush.map { _.content })
        }
    def brush: Parser[KChunk] = """[ ]*[^\n]+""".r ^^ { b ⇒ TextChunk(b.trim) }
    def fence: Parser[KChunk] = "```" ^^ { _ ⇒ EmptySpace("") }
    def unquotedTextLine: Parser[KChunk] = """(?!```)[^\n]+\n""".r ^^ { TextChunk(_) }
    private def foldedString(texts: List[KChunk]): String = ("" /: texts)((current, text) ⇒ current + text.content)
  }
  trait Discounter extends KDiscounter {
    override def newChunkParser: KChunkParser = new KChunkParser with ChunkParser
    override def blockToXHTML: Block ⇒ xml.Node = block ⇒ block match {
      case CodeBlock(text, _, language) ⇒
        fencedChunkToXHTML(text, language)
      case _ ⇒
        super.blockToXHTML(block)
    }
    def fencedChunkToXHTML(text: Text, language: Option[String]) =
      <pre><code class={
        language.map { "prettyprint lang-" + _ }.getOrElse("")
      }>{ text.content }</code></pre>
  }

  case class Chunk(val content: String, language: Option[String]) extends KChunk {
    def appendNewBlock(list: collection.mutable.ListBuffer[Block],
      remaining: List[(KChunk, Seq[Span], Position)],
      spans: Seq[Span], position: Position,
      discounter: KDiscounter) {
      list += CodeBlock(Text(content), position, language)
    }
  }
  case class CodeBlock(text: Text, position: Position, language: Option[String]) extends Block
}
