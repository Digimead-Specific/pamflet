/**
 * Pamflet Knockoff - Extensions to the Knockoff Markdown parser.
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
import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }

trait FencedDiscounter extends Discounter {
  override def newChunkParser : ChunkParser =
    new ChunkParser with FencedChunkParser
  override def blockToXHTML: Block => xml.Node = block => block match {
    case FencedCodeBlock(text, _, language) =>
      fencedChunkToXHTML(text, language)
    case _ => super.blockToXHTML(block)
  }
  def fencedChunkToXHTML(text: Text, language: Option[String]) =
    <pre><code class={
      language.map { "prettyprint lang-" + _ }.getOrElse("")
    }>{ text.content }</code></pre>
}

trait FencedChunkParser extends ChunkParser {
  override def chunk : Parser[ Chunk ] = {
    horizontalRule | leadingStrongTextBlock | leadingEmTextBlock | bulletItem |
    numberedItem | indentedChunk | header | blockquote | linkDefinition |
    htmlBlock | fencedChunk | textBlockWithBreak | textBlock | emptyLines | emptySpace
  }

  def fencedChunk : Parser[ Chunk ] =
    fence ~> opt(brush) ~ emptyLine ~
      rep1(unquotedTextLine | emptyLine) <~ fence <~ emptyLine ^^ {
        case (brush ~ _) ~ lines =>
          FencedChunk(foldedString(lines), brush.map { _.content })
      }

  def brush : Parser[Chunk] =
    """[ ]*[^\n]+""".r ^^ { b => TextChunk(b.trim) }

  def fence : Parser[Chunk] =
    "```" ^^ { _ => EmptySpace("") }

  def unquotedTextLine : Parser[ Chunk ] =
    """(?!```)[^\n]+\n""".r ^^ { TextChunk(_) }

  private def foldedString( texts : List[ Chunk ] ) : String =
    ( "" /: texts )( (current, text) => current + text.content )
}

case class FencedChunk(val content: String, language: Option[String])
extends Chunk {
  def appendNewBlock( list : collection.mutable.ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += FencedCodeBlock(Text(content), position, language)
  }
}

case class FencedCodeBlock(text: Text, position: Position,
                           language: Option[String]) extends Block
