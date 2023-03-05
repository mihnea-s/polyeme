package org.polyeme

import scala.io.{Source, StdIn}
import scala.io.AnsiColor.*

@main def main(files: String*) = {
  files.length match {
    case 0 => promptLoop()
    case 1 => runFile(files.head)
    case _ => ()
  }
}

def promptLoop(): Unit = {
  var unbalancedParens = 0
  var textInput = StringBuilder()

  var parser = Parser()
  var interp = Preloaded.interpreter()

  while true do {
    textInput.setLength(0)

    while {
      val line = StdIn.readLine("$ ")
      if line == null then return ()

      unbalancedParens += line.count(_ == '(')
      unbalancedParens -= line.count(_ == ')')

      textInput.append(line)
      unbalancedParens != 0
    } do ()

    parser(textInput.mkString).map(interp.evaluate).flatten match {
      case Right(value)                    => println(s"$BLUE$value$RESET")
      case Left(Terminated(code, message)) => println(message); return ()
      case Left(value)                     => println(s"$RED$value$RESET")
    }
  }
}

def runFile(filename: String): Unit = {
  val file = Source.fromFile(filename)
  val parser = Parser()
  var interp = Preloaded.interpreter()

  parser(file.mkString).map(interp.evaluate).flatten match {
    case Right(value) => println(s"$value")
    case Left(value)  => println(s"$RED$value$RESET")
  }

  file.close()
}
