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
import scala.util.matching.Regex

trait SmartyDiscounter extends Discounter {
  override def createSpanConverter(
    linkDefinitions: Seq[LinkDefinitionChunk]) =
    new SpanConverter(linkDefinitions) with SmartySpanConverter
}

trait SmartySpanConverter extends SpanConverter {
  override def apply( chunk : Chunk ) : Seq[Span] = {
    chunk match {
      case IndentedChunk(content)  => List( new Text(content) )
      case FencedChunk(content, _) => List( new Text(content) )
      case _ => convert( chunk.content, Nil )
    }
  }

  val punctClass = """[!\"#\$\%'()*+,-.\/:;<=>?\@\[\\\]\^_`{|}~]"""
  val closeClass = """[^\ \t\r\n\[\{\(\-]"""
  def smartyMatchers : List[ String => Option[SpanMatch] ] = List(
    // special case for quote as first character, then punctuation
    replacer("""^'(?=%s\B)""".format(punctClass).r, "‘"),
    replacer("""^"(?=%s\B)""".format(punctClass).r, "“"),
    // normal opening quote cases
    replacer("""(?<=\s|--|—)'(?=\w)""".r, "‘"),
    replacer("""(?<=\s|--|—)"(?=\w)""".r, "“"),
    // closing quotes
    replacer("""(?<=%s)?'(?=\s|\w|%s)""".format(closeClass, punctClass).r, "’"),
    replacer("""(?<=%s)?"(?=\s|%s)""".format(closeClass, punctClass).r, "”"),
    // assume everything else is opening
    replacer("'".r, "‘"),
    replacer("\"".r, "“"),
    replacer("--".r, "—"),
    replacer("""\.\.\.""".r, "…")
  )
  override def matchers =  smartyMatchers ::: super.matchers

  private def replacer(r: Regex, smarted: String)(source : String) = {
    r.findFirstMatchIn( source ).map { qmatch =>
      val before = qmatch.before.toOption.map( Text(_) )
      val html = HTMLSpan(smarted)
      SpanMatch( qmatch.start, before, html, qmatch.after.toOption )
    }
  }
}
