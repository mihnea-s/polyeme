import {
  Datum,
  DatumKind,
  Environment,
  JSFunction,
  pair,
  Pair,
  Procedure,
  unpair,
} from './datum';

export class RuntimeError {
  constructor(public description: string) { }
}

export class Interpreter {
  constructor(private env = [new Environment()]) { }

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

    let returnValue = fn.body.map(this.tryEvaluate).reverse().shift();

    this.env.pop();

    if (returnValue) {
      return returnValue;
    } else {
      throw new RuntimeError('invalid interpreter state');
    }
  }

  private evalPair(pair: Pair): Datum {
    if (pair.left.kind === DatumKind.Procedure) {
      return this.applyProc(
        pair.left,
        unpair(pair.right).map(this.tryEvaluate)
      );
    }

    if (pair.left.kind !== DatumKind.Symbol) {
      throw new RuntimeError('invalid call to non-function');
    }

    switch (pair.left.value) {
      case 'quote':
        return pair.right;

      case 'if':
        throw new RuntimeError('undefined');

      case 'when':
        throw new RuntimeError('undefined');

      case 'unless':
        throw new RuntimeError('undefined');

      case 'cond':
        throw new RuntimeError('undefined');

      case 'case':
        throw new RuntimeError('undefined');

      case 'let':
        throw new RuntimeError('undefined');

      case 'def':
        throw new RuntimeError('undefined');

      case 'defn':
        throw new RuntimeError('undefined');

      case 'defmacro':
        throw new RuntimeError('undefined');
    }

    const fn = this.environment().read(pair.left.value);

    if (![DatumKind.Procedure, DatumKind.JSFunction].includes(fn.kind)) {
      throw new RuntimeError(`undefined function ${pair.left.value}`);
    }

    let args = unpair(pair.right);

    if (pair.left.value.endsWith('!')) {
      args = args.map(this.tryEvaluate);
    } else {
      if (args.length < 0 && args[0].kind! == DatumKind.Symbol) {
        throw new RuntimeError('invalid arguments for setter');
      } else {
        args = [args[0], ...args.slice(1).map(this.tryEvaluate)];
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
      this.environment().write(name, {
        kind: DatumKind.JSFunction,
        value: jsf,
      });
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
