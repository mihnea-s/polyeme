package org.polyeme

import scala.collection.immutable.HashMap

object Preloaded {
  def interpreter(): Interpreter = {
    val preloaded = HashMap(
      "set!" -> FunctionData(set),
      "exit" -> FunctionData(exit),
      "load" -> FunctionData(load),
      "eval" -> FunctionData(eval),
      "apply" -> FunctionData(apply),
      "system" -> FunctionData(system),
      "void" -> FunctionData(void)
    )

    Interpreter(Environment(preloaded))
  }

  private def set(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    args match {
      case List(Symbol(name), value) => env.mutate(name, value)
      case _                         => Left(InvalidParameter(Datum.pair(args)))
    }
  }

  private def exit(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    val (code, message) = args match {
      case List(IntData(code), StringData(msg))  => (code, s"Exit was called: $msg")
      case List(RealData(code), StringData(msg)) => (code.toInt, s"Exit was called: $msg")
      case List(StringData(msg))                 => (1, s"Exit was called: $msg")
      case List(value)                           => (1, s"Exit was called: $value")
      case _                                     => (1, "Exit was called, terminating program")
    }

    return Left(Terminated(code, message))
  }

  private def load(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    return Right(Void)
  }

  private def eval(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    return Right(Void)
  }

  private def apply(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    return Right(Void)
  }

  private def system(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    return Right(Void)
  }

  private def void(env: Environment, args: List[Datum]): Either[Error, Datum] = {
    return Right(Void)
  }
}
