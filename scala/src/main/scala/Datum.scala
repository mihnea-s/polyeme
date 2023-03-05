package org.polyeme

import scala.collection.immutable.*

sealed abstract class Datum {
  def truthy(): Boolean = this match {
    case BoolData(false) => false
    case _               => true
  }

  def unpair(): List[Datum] = this match {
    case Symbol("()")        => Nil
    case Pair(first, second) => first :: second.unpair()
    case other               => other :: Nil
  }

  def unpairN(n: Int): Either[Error, List[Datum]] = unpair() match {
    case l if l.length == n => Right(l)
    case l                  => Left(InvalidArgumentCount(l.length, n))
  }

  override def toString(): String = this match {
    case Void                      => "#<void>"
    case Symbol(name)              => name
    case BoolData(true)            => "#t"
    case BoolData(false)           => "#f"
    case CharData(value)           => s"#'$value"
    case IntData(value)            => value.toString
    case RealData(value)           => value.toString
    case StringData(value)         => s"\"$value\""
    case VectorData(value)         => s"#($value)"
    case HashData(_)               => "#<hash>"
    case PortData()                => "#<port>"
    case Pair(first, second)       => s"($first $second)"
    case EnvData(_)                => "#<environment>"
    case FunctionData(_)           => "#<native-proc>"
    case ProcedureData(_, _, _, _) => s"#<procedure>"
  }
}

object Datum {
  def pair(list: List[Datum]): Datum = {
    list.foldLeft(Symbol.Nil: Datum)(Pair.apply)
  }
}

case object Void extends Datum

case class Symbol(name: String) extends Datum

case object Symbol {
  val Nil = Symbol("()")
  val VaArgs = Symbol("..")
}

case class BoolData(value: Boolean) extends Datum

case class CharData(value: Char) extends Datum

case class IntData(value: Int) extends Datum

case class RealData(value: Double) extends Datum

case class StringData(value: String) extends Datum

case class VectorData(value: Vector[Datum]) extends Datum

case class HashData(value: HashMap[String, Datum]) extends Datum

case class PortData() extends Datum

class PortReaderData(val value: java.io.Reader) extends PortData

object PortReaderData {
  def unapply(datum: Datum): Option[java.io.Reader] =
    datum match
      case d: PortReaderData => Some(d.value)
      case _                 => None
}

class PortWriterData(val value: java.io.Writer) extends PortData

object PortWriterData {
  def unapply(datum: Datum): Option[java.io.Writer] =
    datum match
      case d: PortWriterData => Some(d.value)
      case _                 => None
}

case class Pair(first: Datum, second: Datum) extends Datum

case class EnvData(value: Environment) extends Datum

case class FunctionData(fn: (Environment, List[Datum]) => Either[Error, Datum]) extends Datum

case class ProcedureData(
    params: List[String],
    varParams: Option[String],
    body: List[Datum],
    closure: Environment
) extends Datum

sealed class Environment(private var env: HashMap[String, Datum]) {
  def this() = {
    this(HashMap())
  }

  def read(key: String): Either[Error, Datum] = {
    return env.get(key).toRight(UnboundVar(key))
  }

  def write(key: String, value: Datum): Datum = {
    env = env + ((key, value))
    return value
  }

  def mutate(key: String, value: Datum): Either[Error, Datum] = {
    env.get(key) match {
      case Some(_) => Right(write(key, value))
      case None    => Left(UnboundVar(key))
    }
  }

  def extend(vars: List[(String, Datum)]): Environment = {
    return new Environment(env ++ vars)
  }
}

sealed abstract class Error {
  override def toString(): String = this match
    case UnboundVar(name)                    => s"Unbound variable '$name'"
    case ParseFailed(message, at)            => s"Parsing failed, $message, at $at"
    case BadSpecialForm(form)                => s"Bad special form at '$form'"
    case Terminated(code, message)           => s"$message ($code)"
    case NotCallable(datum)                  => s"Not a procedure: '$datum'"
    case InvalidParameter(param)             => s"'$param' is an invalid parameter"
    case InvalidArgumentCount(count, expect) => s"Expected $expect parameters but got $count"
    case MultipleErrors(errors) => s"Multiple errors occurred: ${errors.mkString("\n")}"
}

case class UnboundVar(val name: String) extends Error

case class ParseFailed(val message: String, val at: String) extends Error

case class BadSpecialForm(val form: Datum) extends Error

case class Terminated(val code: Int, val message: String) extends Error

case class NotCallable(val datum: Datum) extends Error

case class InvalidParameter(val param: Datum) extends Error

case class InvalidArgumentCount(val count: Int, val expect: Int) extends Error

case class MultipleErrors(val errors: List[Error]) extends Error
