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

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.Properties

import scala.annotation.tailrec

import org.digimead.booklet.content.Globalized

trait Storage {
  /** User provided properties. */
  val properties: Properties
  /** Globalized content. */
  def globalized: Globalized
}

object Storage {
  def copy(r: InputStream, w: OutputStream) {
    @tailrec def doCopy: Unit = {
      val byte = r.read()
      if (byte != -1) {
        w.write(byte)
        doCopy
      }
    }
    doCopy
    w.flush()
  }
  def write(path: String, target: File, r: InputStream) {
    val file = new File(target, path)
    new File(file.getParent).mkdirs()
    val w = new FileOutputStream(file)
    copy(r, w)
    r.close()
    w.close()
  }
  def writeString(path: String, contents: String, target: File) {
    write(path, target, new ByteArrayInputStream(contents.getBytes("UTF-8")))
  }
}
