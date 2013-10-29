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
import java.util.Properties

import org.digimead.booklet.storage.FileStorage
import org.slf4j.LoggerFactory

class Application extends xsbti.AppMain {
  def run(config: xsbti.AppConfiguration) = Exit(Application.run(config.arguments))
  case class Exit(val code: Int) extends xsbti.Exit
}

object Application {
  lazy val log = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]) = System.exit(run(args))
  def run(args: Array[String]) = args match {
    case args if args.size >= 2 && args.takeRight(2).forall(dir(_).nonEmpty) ⇒
      val Array(input, output) = args.takeRight(2).map(dir(_).get)
      val properties = buildProperties(args.dropRight(2))
      Produce(storage(input, properties).globalized, output)
      log.info("Wrote booklet to " + output); 0
    case args if args.size >= 1 && dir(args.last).nonEmpty ⇒
      val input = dir(args.last).get
      val properties = buildProperties(args.dropRight(1))
      preview(input, properties)
    case other ⇒
      val properties = buildProperties(args)
      dir("docs") match {
        case Some(input) ⇒
          preview(input, properties)
        case _ ⇒
          println("""Usage: pf [SRC] [DEST]
                    |
                    |Default SRC is ./docs""".stripMargin); 1
      }
  }
  def preview(input: File, properties: Properties) = {
    Preview(storage(input, properties).globalized).run { server ⇒
      unfiltered.util.Browser.open(
        "http://127.0.0.1:%d/".format(server.port))
      println("\nPreviewing `%s`. Press CTRL+C to stop.".format(input.getName()))
      if (properties.containsKey(Booklet.Options.optionVerbose))
        log.info("Process " + input.getCanonicalPath())
    }; 0
  }

  protected def dir(path: String) = new File(path) match {
    case file if file.exists && file.isDirectory ⇒ Some(file)
    case _ ⇒ None
  }
  protected def file(path: String) = new File(path) match {
    case file if file.exists && file.isFile() ⇒ Some(file)
    case _ ⇒ None
  }
  protected def storage(dir: File, properties: Properties) = FileStorage(dir, properties)
  protected def buildProperties(args: Array[String]): Properties = {
    val properties: Properties = new Properties
    Booklet.mergeWithFiles(properties,
      args.flatMap(f ⇒ if (f.startsWith("@")) file(f.drop(1)) else None): _*)
    if (args.contains("-v")) {
      properties.setProperty(Booklet.Options.optionVerbose, "Y")
      println(properties.containsKey(Booklet.Options.optionVerbose))
      if (args.filter(_ == "-v").size > 1) {
        try {
          val simpleLoggerClass = Class.forName("org.slf4j.impl.SimpleLogger")
          val defaultLogLevelField = simpleLoggerClass.getDeclaredField("DEFAULT_LOG_LEVEL")
          if (!defaultLogLevelField.isAccessible())
            defaultLogLevelField.setAccessible(true)
          defaultLogLevelField.setInt(null, 0)
        } catch {
          case e: ClassNotFoundException ⇒
        }
      }
    }

    properties
  }
}
