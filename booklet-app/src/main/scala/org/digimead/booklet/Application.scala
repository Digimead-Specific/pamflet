/**
 * Booklet application for previewing and publishing project documentation.
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

package org.digimead.booklet

import java.io.File
import org.digimead.booklet.storage.FileStorage

class Application extends xsbti.AppMain {
  def run(config: xsbti.AppConfiguration) = Exit(Application.run(config.arguments))
  case class Exit(val code: Int) extends xsbti.Exit
}

object Application {
  def main(args: Array[String]) = System.exit(run(args))
  def run(args: Array[String]) = {
    args match {
      case Array(Dir(input), Dir(output)) ⇒
        Produce(storage(input).globalized, output)
        println("Wrote pamflet to " + output)
        0
      case Array(Dir(dir)) ⇒ preview(dir)
      case Array() ⇒
        "docs" match {
          case Dir(docs) ⇒ preview(docs)
          case _ ⇒
            println("""Usage: pf [SRC] [DEST]
                    |
                    |Default SRC is ./docs""".stripMargin)
            1
        }
      case _ ⇒
        println("Input paths must be directories")
        1
    }
  }
  def preview(dir: File) = {
    Preview(storage(dir).globalized).run { server ⇒
      unfiltered.util.Browser.open(
        "http://127.0.0.1:%d/".format(server.port))
      println("\nPreviewing `%s`. Press CTRL+C to stop.".format(dir))
    }
    0
  }

  private def storage(dir: File) = FileStorage(dir)

  object Dir {
    def unapply(path: String) = {
      val file = new File(path)
      if (file.exists && file.isDirectory)
        Some(file)
      else None
    }
  }
}
