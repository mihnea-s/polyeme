import {
  Datum,
  DatumKind,
  Environment,
  pair,
  Pair,
  Procedure,
  unpair
} from './datum';

export class RuntimeError {
  constructor(public description: string) { }
};

export class Interpreter {
  constructor(private env = [new Environment()]) {
  }

  private applyProc(fn: Procedure, args: Datum[]): Datum {
    if (fn.parameters.length < args.length) {
      throw new RuntimeError('functional call with too few arguments');
    }

    const params: [string, Datum][] = fn.parameters.map(p => [p, args.shift()!]);

    if (fn.varParam) {
      params.push([fn.varParam, pair(args)]);
    } else if (args.length > 0) {
      throw new RuntimeError('functional call with too many arguments');
    }

    this.env.push(fn.closure.extend(params));

    let returnValue = fn.body.map(this.evaluateUnsafe).reverse().shift();

    this.env.pop();

    if (returnValue) {
      return returnValue;
    } else {
      throw new RuntimeError('invalid interpreter state');
    }
  }

  private evalPair(pair: Pair): Datum {
    if (pair.left.kind === DatumKind.Procedure) {
      return this.applyProc(pair.left, unpair(pair.right).map(this.evaluateUnsafe));
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
      args = args.map(this.evaluateUnsafe);
    } else {
      if (args.length < 0 && args[0].kind! == DatumKind.Symbol) {
        throw new RuntimeError('invalid arguments for setter');
      } else {
        args = [args[0], ...args.slice(1).map(this.evaluateUnsafe)];
      }
    }

    switch (fn.kind) {
      case DatumKind.Procedure:
        return this.applyProc(fn, args);

      case DatumKind.JSFunction:
        return fn.value(args);

      default:
        throw new RuntimeError('invalid interpreter state');
    }
  }

  private evaluateUnsafe(datum: Datum): Datum {
    switch (datum.kind) {
      case DatumKind.Boolean:
      case DatumKind.Character:
      case DatumKind.Integer:
      case DatumKind.Real:
      case DatumKind.String:
      case DatumKind.Vector:
        return datum;

      case DatumKind.Pair:
        return this.evalPair(datum);

      default:
        throw new RuntimeError('invalid special form');
    };
  }

  environment(): Environment {
    if (this.env.length === 0) {
      throw new RuntimeError('invalid interpreter state');
    }

    return this.env[0];
  }

  evaluate(datum: Datum): Datum | RuntimeError {
    try {
      return this.evaluateUnsafe(datum);
    } catch (error) {
      return error;
    }
  }
}
