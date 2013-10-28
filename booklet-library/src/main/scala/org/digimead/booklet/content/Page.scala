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

package org.digimead.booklet.content

import java.util.Properties

import org.digimead.booklet.template.Template

trait Page {
  def localPath: String
  def name: String
  def prettifyLangs: Set[String]
  def properties: Properties
  def referencedLangs: Set[String]
  def template: Template

  /** Get property for given key if present. */
  def getProperty(key: String) = Option(properties.get(key)) map { _.toString }
  /** Set property for given key if present. */
  def setProperty(key: String, value: String) = properties.setProperty(key, value)
}
