package org.polyeme

import scala.util.parsing.combinator.*

class Parser extends RegexParsers {
  override protected val whiteSpace = """(\s|\s*;.*)+""".r

  // Parse possible symbol names
  private def symbol = {
    val syms = "!#$%&|*+-/:<=>?@^_~"
    s"""[$syms\\p{L}][$syms\\p{L}\\p{N}]*""".r ^^ { Symbol(_) }
  }

  // Parse string literal with escaped characters
  private def string = "\"" ~> """(\\.|[^"\\])*""".r <~ "\"" ^^ { StringData(_) }

  // Parse number
  private def number = (numberBin | numberHex) ^^ { IntData(_) } | numberDec
  private def numberBin = "0b" ~> "[01]+".r ^^ { Integer.parseInt(_, 2) }
  private def numberHex = "0x" ~> "[a-f\\d]+".r ^^ { Integer.parseInt(_, 16) }
  private def numberDec = """\d+(\.\d+)?""".r ^^ { n => RealData(n.toDouble) }

  // Parse variable args function parameter
  private def vaargs = ".." ^^^ Symbol.VaArgs

  // Parse hash literals
  private def hash = "#" ~> (hashTrue | hashFalse | hashChar | hashVec)
  private def hashTrue = "t" ^^^ BoolData(true)
  private def hashFalse = "f" ^^^ BoolData(false)
  private def hashChar = "'" ~> ".".r ^^ { _.charAt(0) } ^^ { CharData(_) }
  private def hashVec = "(" ~> expr.* <~ ")" ^^ { v =>
    VectorData(Vector.from(v))
  }

  // Parse a literal atom
  private def atom = hash | vaargs | string | number | symbol

  // Parse quoted expressions that won't be automatically evaluated:
  //    '(2 + 2)
  private def quoted = "'" ~> expr ^^ { Pair(Symbol("quote"), _) }

  // Parse a list of expressions separated by spaces:
  //    (a b c d)
  private def pair = expr.* ^^ { _.foldRight(Symbol.Nil: Datum)(Pair.apply) }

  // Parse expressions separated by dots:
  //    (car . (a . (b . cdr)))
  private def dotted = (expr <~ ".") ~ expr ^^ { case car ~ cdr =>
    Pair(car, cdr)
  }

  // Parse expressions enclosed by brackets and parends:
  //    (a . (b c)) or [a b c]
  private def enclosed = "(" ~> (pair | dotted) <~ ")" | "[" ~> pair <~ "]"

  private def expr: Parser[Datum] = quoted | enclosed | atom

  def apply(input: CharSequence): Either[org.polyeme.Error, List[Datum]] = {
    return parse(phrase(expr.*), input) match {
      case Success(result, _)     => Right(result)
      case NoSuccess.I(msg, next) => Left(ParseFailed(msg, s"${next.pos.line}:${next.pos.column}"))
    }
  }
}
