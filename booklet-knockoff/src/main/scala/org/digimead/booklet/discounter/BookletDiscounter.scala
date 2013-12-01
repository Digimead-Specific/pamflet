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

import java.util.Properties

import scala.util.DynamicVariable

import com.tristanhunt.knockoff.Block
import com.tristanhunt.knockoff.{ ChunkParser ⇒ KChunkParser }
import com.tristanhunt.knockoff.Discounter
import com.tristanhunt.knockoff.LinkDefinitionChunk
import com.tristanhunt.knockoff.SpanConverter
import com.tristanhunt.knockoff.Text

trait BookletDiscounter extends Discounter {
  override lazy val newChunkParser: KChunkParser = new KChunkParser with ChunkParser
  override lazy val blockToXHTML: Block ⇒ xml.Node = block ⇒ block match {
    case ChunkParser.CodeBlock(text, _, language) ⇒
      fencedChunkToXHTML(text, language)
    case _ ⇒
      super.blockToXHTML(block)
  }

  def fencedChunkToXHTML(text: Text, language: Option[String]) = {
    // Settings.codeLineNums
    val fencedClasses = if (BookletDiscounter.pageProperties.value.getProperty("codeLineNums", "N").toUpperCase() == "Y") "prettyprint linenums" else "prettyprint"
    <pre class={ fencedClasses }><code class={ language.map { "prettyprint lang-" + _ }.getOrElse("") }>{ text.content }</code></pre>
  }
  /**
   * Parses and returns our best guess at the sequence of blocks. It will
   * never fail, just log all suspicious things.
   */
  def knockoff(source: java.lang.CharSequence, pageProperties: Properties): Seq[Block] =
    BookletDiscounter.pageProperties.withValue(pageProperties)(super.knockoff(source))
 // override def createSpanConverter(linkDefinitions: Seq[LinkDefinitionChunk]) = new SpanConverter(linkDefinitions) with SmartySpanConverter
}

object BookletDiscounter extends BookletDiscounter with Headers.Identified with Imgs.Html5 {
  /** Thread local variable with current page properties. */
  val pageProperties = new DynamicVariable(new Properties)
}
