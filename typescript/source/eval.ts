import {
  Datum,
  DatumKind,
  Environment,
  isTruthy,
  Symbol,
  isFalsy,
  mkJSFunction,
  mkVoid,
  pair,
  Pair,
  Procedure,
  unpair,
  cast,
  mkProcedure,
} from './datum';

export class RuntimeError {
  constructor(public description: string) { }
}

export class Interpreter {
  constructor(private env = [new Environment()]) { }

  private unpairExact(datum: Datum, count: number): [...Datum[]] {
    const unpaired = unpair(datum);

    if (unpaired.length != count) {
      throw new RuntimeError(
        `expectd ${count} arguments for special form,`
        + ` got ${unpaired.length}`
      );
    }

    return unpaired;
  }

  private applyProc(fn: Procedure, args: Datum[]): Datum {
    if (fn.parameters.length < args.length) {
      throw new RuntimeError('functional call with too few arguments');
    }

    const params: [string, Datum][] = fn.parameters.map((p) => [
      p,
      args.shift()!,
    ]);

    if (fn.varParam) {
      params.push([fn.varParam, pair(args)]);
    } else if (args.length > 0) {
      throw new RuntimeError('functional call with too many arguments');
    }

    this.env.push(fn.closure.extend(params));

    let returnValue = fn.body.map((d) => this.tryEvaluate(d)).reverse().shift();

    this.env.pop();

    if (returnValue) {
      return returnValue;
    } else {
      throw new RuntimeError('invalid interpreter state');
    }
  }

  private specialFormLambda(datum: Datum): Datum {
    let [params, ...body] = unpair(datum);

    if (!params) {
      throw new RuntimeError('invalid lambda special form');
    }

    return this.evalLambda(params, body);
  }

  private specialFormIf(datum: Datum): Datum {
    const [cond, then, alt] = this.unpairExact(datum, 3);

    if (isTruthy(this.tryEvaluate(cond))) {
      return this.tryEvaluate(then);
    } else {
      return this.tryEvaluate(alt);
    }
  }

  private specialFormWhen(datum: Datum): Datum {
    const [cond, then] = this.unpairExact(datum, 2);

    if (isTruthy(this.tryEvaluate(cond))) {
      return this.tryEvaluate(then);
    } else {
      return mkVoid();
    }
  }

  private specialFormUnless(datum: Datum): Datum {
    const [cond, alt] = this.unpairExact(datum, 2);

    if (isFalsy(this.tryEvaluate(cond))) {
      return this.tryEvaluate(alt);
    } else {
      return mkVoid();
    }
  }

  private specialFormCond(datum: Datum): Datum {
    const conds = unpair(datum);

    if (conds.length % 2 !== 0) {
      throw new RuntimeError('cond requires even number of arguments');
    }

    for (const [cond, conseq] of
      [...Array(conds.length / 2).keys()]
        .map(i => [conds[2 * i], conds[2 * i + 1]])
    ) {
      const condIsTrue =
        (cond.kind === DatumKind.Symbol && cond.value === 'else')
        || isTruthy(this.tryEvaluate(cond));

      if (condIsTrue) {
        return this.tryEvaluate(conseq);
      }
    }

    return mkVoid();
  }

  private specialFormLet(datum: Datum): Datum {
    const [bindingsVar, ...body] = unpair(datum);

    if (!bindingsVar) {
      throw new RuntimeError('invalid let special form');
    }

    const bindings = unpair(bindingsVar);

    const letEnv = this.environment().extend(
      [...Array(bindings.length / 2).keys()]
        .map(i => [bindings[2 * i], bindings[2 * i + 1]])
        .map(([name, value]) => [cast<Symbol>(name, DatumKind.Symbol).value, value])
    );

    this.env.push(letEnv);

    let value = body
      .map(expr => this.tryEvaluate(expr))
      .reduce((_, v) => v);

    this.env.pop();

    return value;
  }

  private specialFormDef(datum: Datum): Datum {
    let [name, value] = this.unpairExact(datum, 2);

    this.environment().write(
      cast<Symbol>(name, DatumKind.Symbol).value, value
    );

    return mkVoid();
  }

  private specialFormDefn(datum: Datum): Datum {
    let [name, params, ...body] = unpair(datum);

    if (!(name && params)) {
      throw new RuntimeError('invalid defn special form');
    }

    this.environment().write(
      cast<Symbol>(name, DatumKind.Symbol).value,
      this.evalLambda(params, body),
    );

    return mkVoid();
  }

  private evalLambda(params: Datum, body: Datum[]): Datum {
    const closure = this.environment();
    const paramNames = unpair(params).map(
      param => cast<Symbol>(param, DatumKind.Symbol).value
    );

    return mkProcedure(body, null, paramNames, closure);
  }

  private evalPair(pair: Pair): Datum {
    if (pair.left.kind === DatumKind.Procedure) {
      return this.applyProc(
        pair.left,
        unpair(pair.right).map((d) => this.tryEvaluate(d))
      );
    }

    if (pair.left.kind !== DatumKind.Symbol) {
      throw new RuntimeError('invalid call to non-function');
    }

    switch (pair.left.value) {
      case 'quote': {
        return pair.right;
      }

      case 'lambda': {
        return this.specialFormLambda(pair.right);
      }

      case 'if': {
        return this.specialFormIf(pair.right);
      }

      case 'when': {
        return this.specialFormWhen(pair.right);
      }

      case 'unless': {
        return this.specialFormUnless(pair.right);
      }

      case 'cond': {
        return this.specialFormCond(pair.right);
      }

      case 'case': {
        throw new RuntimeError('unimplemented');
      }

      case 'let': {
        return this.specialFormLet(pair.right);
      }

      case 'def': {
        return this.specialFormDef(pair.right);
      }

      case 'defn': {
        return this.specialFormDefn(pair.right);
      }

      case 'defmacro': {
        throw new RuntimeError('unimplemented');
      }
    }

    const fn = this.environment().read(pair.left.value);

    if (![DatumKind.Procedure, DatumKind.JSFunction].includes(fn.kind)) {
      throw new RuntimeError(`undefined function ${pair.left.value}`);
    }

    let args = unpair(pair.right);

    if (!pair.left.value.endsWith('!')) {
      args = args.map((d) => this.tryEvaluate(d));
    } else {
      if (args.length == 0 || args[0].kind !== DatumKind.Symbol) {
        throw new RuntimeError('invalid arguments for setter');
      } else {
        args = [args[0], ...args.slice(1).map((d) => this.tryEvaluate(d))];
      }
    }

    switch (fn.kind) {
      case DatumKind.Procedure:
        return this.applyProc(fn, args);

      case DatumKind.JSFunction:
        return fn.value(this, args);

      default:
        throw new RuntimeError('invalid interpreter state');
    }
  }

  /**
   * Preload custom JavaScript bindings into the runtime.
   * @param defs list of name, function pairs to be preloaded 
   */
  preload(defs: [string, (i: Interpreter, a: Datum[]) => Datum][]) {
    for (const [name, jsf] of defs) {
      this.environment().write(name, mkJSFunction(jsf));
    }
  }

  /**
   * Get the current environment of the interpreter.
   * @returns environment being used to evaluate expressions
   */
  environment(): Environment {
    if (this.env.length === 0) {
      throw new RuntimeError('invalid interpreter state');
    }

    return this.env[0];
  }

  /**
   * Try to evaluate an expreesion, throws if fails.
   * @param   {Datum}         datum the expression to be evaluated
   * @throws  {RuntimeError}  if evaluation fails
   * @returns {Datum}         the value of the evaluated expression
   */
  tryEvaluate(datum: Datum): Datum {
    switch (datum.kind) {
      case DatumKind.Void:
      case DatumKind.Boolean:
      case DatumKind.Character:
      case DatumKind.Integer:
      case DatumKind.Real:
      case DatumKind.String:
      case DatumKind.Vector:
      case DatumKind.Hash:
      case DatumKind.Environment:
      case DatumKind.Port:
      case DatumKind.JSFunction:
      case DatumKind.Procedure:
        return datum;

      case DatumKind.Symbol:
        return this.environment().read(datum.value);

      case DatumKind.Pair:
        return this.evalPair(datum);

      default:
        throw new RuntimeError('invalid special form');
    }
  }

  /**
   * Wrapper around tryEvaluate that does not throw.
   * @param   {Datum} datum the expression to be evaluated
   * @returns {Datum | RuntimeError} the value of the evaluated expression or the error
   */
  evaluate(datum: Datum): Datum | RuntimeError {
    try {
      return this.tryEvaluate(datum);
    } catch (error) {
      return error;
    }
  }

  /**
   * Evaluate a value in the given environment.
   * @param {Datum}       datum the expression to be evaluated
   * @param {Environment} env   the environment to be used in evaluation
   * @returns {Datum | RuntimeError} the value of the evaluated expression or the error
   */
  evaluateWith(datum: Datum, env: Environment): Datum | RuntimeError {
    this.env.push(env);
    const evaluated = this.evaluate(datum);
    this.env.pop();

    return evaluated;
  }
}
