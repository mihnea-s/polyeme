package org.polyeme

class Interpreter(private var env: Environment) {
  private def conditional(
      cond: Datum,
      conseq: Datum,
      altern: Datum
  ): Either[Error, Datum] = {
    evaluate(cond) match {
      case Right(d) if d.truthy() => evaluate(conseq)
      case Right(_)               => evaluate(altern)
      case Left(error)            => Left(error)
    }
  }

  private def conditional(branches: List[List[Datum]]): Either[Error, Datum] = {
    branches
      .foldLeft(None: Option[Either[Error, Datum]]) {
        case (Some(d), _)          => Some(d)
        case (None, List(default)) => Some(evaluate(default))
        case (None, List(cond, conseq)) =>
          cond.truthy() match {
            case true  => Some(evaluate(conseq))
            case false => None
          }
        // Unreachable
        case _ => ???
      }
      .getOrElse(Right(Void))
  }

  private def mkProc(paramData: Datum, body: Datum): Either[Error, Datum] = {
    val (paramSyms, errors) = paramData
      .unpair()
      .partitionMap {
        case Symbol(name) => Left(name)
        case other        => Right(InvalidParameter(other))
      }

    if (!errors.isEmpty) {
      return Left(MultipleErrors(errors))
    }

    val (params, vaarg) = paramSyms match {
      case params :+ l if l == ".." => (params.init, Some(params.last))
      case other                    => (other, None)
    }

    return Right(ProcedureData(params, vaarg, body.unpair(), env))
  }

  private def callFunc(func: Datum, args: List[Datum]): Either[Error, Datum] = {
    func match {
      case FunctionData(fn) => fn(this.env, args)

      case ProcedureData(params, varParams, body, closure) => {
        if (args.length != params.length && varParams.isEmpty) {
          return Left(InvalidArgumentCount(args.length, params.length))
        }
        if (args.length < params.length) {
          return Left(InvalidArgumentCount(args.length, params.length))
        }

        val vaargs = Datum.pair(args.drop(params.length))

        val procEnv = closure.extend(varParams match
          case None        => params.zip(args)
          case Some(value) => (value, vaargs) :: params.zip(args)
        )

        Interpreter(procEnv).evaluate(body)
      }

      case _ => Left(NotCallable(func))
    }
  }

  def evaluate(data: List[Datum]): Either[Error, Datum] = {
    data.foldRight(Right(Symbol.Nil): Either[Error, Datum]) {
      case (_, err @ Left(_)) => err
      case (expr, Right(_))   => this.evaluate(expr)
    }
  }

  def evaluate(datum: Datum): Either[Error, Datum] = {
    datum match {
      case Symbol(name) => env.read(name)

      case d @ Void          => Right(d)
      case d @ BoolData(_)   => Right(d)
      case d @ CharData(_)   => Right(d)
      case d @ IntData(_)    => Right(d)
      case d @ RealData(_)   => Right(d)
      case d @ StringData(_) => Right(d)
      case d @ VectorData(_) => Right(d)

      case Pair(Symbol("quote"), right)               => Right(right)
      case Pair(Symbol("lambda"), Pair(params, body)) => mkProc(params, body)

      case Pair(Symbol("when"), Pair(cond, Pair(body, Symbol.Nil))) =>
        conditional(cond, body, Void)
      case Pair(Symbol("unless"), Pair(cond, Pair(body, Symbol.Nil))) =>
        conditional(cond, Void, body)
      case d @ Pair(Symbol("if"), args) =>
        args.unpair() match
          case List(a, b, c) => conditional(a, b, c)
          case _             => Left(BadSpecialForm(d))
      case d @ Pair(Symbol("cond"), args) =>
        conditional(args.unpair().grouped(2).toList)

      case Pair(Symbol("case"), args)             => ???
      case Pair(Symbol("let"), Pair(binds, body)) => ???

      case Pair(Symbol("def"), Pair(Symbol(name), Pair(value, Symbol.Nil))) =>
        evaluate(value).map(env.write(name, _))
      case Pair(Symbol("defn"), Pair(Symbol(name), Pair(params, body))) =>
        mkProc(params, body).map(env.write(name, _))
      case Pair(Symbol("defmacro"), Pair(Symbol(name), Pair(params, body))) =>
        ???

      case Pair(setterVal @ Symbol(setter), Pair(varName @ Symbol(_), argsVal))
          if setter.endsWith("!") => {
        val setter = evaluate(setterVal)
        val (errors, args) = argsVal.unpair().partitionMap(evaluate)

        if (errors.nonEmpty) {
          return Left(MultipleErrors(errors))
        }

        setter match {
          case Right(setter) => callFunc(setter, varName :: args)
          case error         => error
        }
      }

      case Pair(funcVal, argsVal) => {
        val func = evaluate(funcVal)
        val (errors, args) = argsVal.unpair().partitionMap(evaluate)

        if (errors.nonEmpty) {
          return Left(MultipleErrors(errors))
        }

        func match {
          case Right(func) => callFunc(func, args)
          case error       => error
        }
      }

      case other => Left(BadSpecialForm(other))
    }
  }
}
