import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop._

import org.polyeme._

class ParserSuite extends FunSuite with ScalaCheckSuite {
  val parser = FunFixture[Parser](
    setup = { test => new Parser() },
    teardown = { _ => {} }
  )

  parser.test("ignore comments") { parser =>
    val input = """
      ; ignore this comment
      ; this one too

      ()
    """

    val expect = List(Symbol("()"))
    assertEquals(parser(input), Right(expect))
  }

  parser.test("parse symbols") { parser =>
    val symbols = List(
      "abc",
      "$50",
      "a!b#%L@?",
      "a000000000000000?"
    )

    val input = symbols.mkString(" ")
    val expect = symbols.map(Symbol(_))

    assertEquals(parser(input), Right(expect))
  }

  parser.test("parse booleans") { parser =>
    assertEquals(parser("#t"), Right(List(BoolData(true))))
    assertEquals(parser("#f"), Right(List(BoolData(false))))
  }

  parser.test("parser properties") { parser =>
    property("can parse chars") {
      forAll { (chr: Char) =>
        assertEquals(parser(s"#'$chr"), Right(List(CharData(chr))))
      }
    }

    property("can parse strings") {
      forAll { (str: String) =>
        val input = s"""
          ; This tests string atoms
          "$str"
        """
        assertEquals(parser(input), Right(List(StringData(str))))
      }
    }

    property("can parse anything") {
      forAll { (input: String) =>
        val _ = parser(input)
      }
    }
  }
}
