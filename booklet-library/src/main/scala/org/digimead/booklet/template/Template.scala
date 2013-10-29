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
import java.util.Properties

import scala.collection.JavaConverters._

import org.antlr.stringtemplate.StringTemplate
import org.fusesource.scalate.TemplateEngine

/**
 * Property like container that consists of a group of booklet template values.
 */
class Template {
  /** Replace template values in input stream with bound properties. */
  def apply(input: CharSequence)(implicit properties: Properties) = {
    val st = new StringTemplate
    st.setTemplate(input.toString)
    st.setAttributes(properties)
    st.toString
  }
  /** Return booklet default language. */
  def defaultLanguage(implicit properties: Properties): String = get(properties, "language") getOrElse "en"
  /** Get booklet languages. */
  def languages(implicit properties: Properties): Seq[String] = get(properties, "languages") match {
    case Some(xs) ⇒ xs.split(",").toSeq map { _.trim }
    case None ⇒ Seq(defaultLanguage)
  }

  /** Return property for given key if present. */
  protected def get(properties: Properties, key: String) = Option(properties.get(key)) map { _.toString }
}

object Template {
  /** Scalate engine. */
  lazy val engine = new TemplateEngine
  /** Temporary directory for booklet. */
  lazy val tmpDirectory = {
    val dir = new File(engine.tmpDirectory, "Booklet-" + System.currentTimeMillis() + System.nanoTime())
    dir.mkdirs()
    dir
  }

  def apply() = new Template()
}
