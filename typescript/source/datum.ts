import { ReadPort, WritePort, ReadWritePort } from './platform';
import { Interpreter, RuntimeError } from './eval';

export type Datum
  = Void
  | Symbol
  | Boolean
  | Character
  | Integer
  | Real
  | String
  | Vector
  | Hash
  | Port
  | Pair
  | Environment
  | JSFunction
  | Procedure;

export enum DatumKind {
  Void,
  Symbol,
  Boolean,
  Character,
  Integer,
  Real,
  String,
  Vector,
  Hash,
  Port,
  Pair,
  Environment,
  JSFunction,
  Procedure,
}

export interface Void {
  kind: DatumKind.Void,
}

export interface Symbol {
  kind: DatumKind.Symbol,
  value: string,
}

export interface Boolean {
  kind: DatumKind.Boolean,
  value: boolean,
}

export interface Character {
  kind: DatumKind.Character,
  value: number;
}

export interface Integer {
  kind: DatumKind.Integer,
  value: number,
}

export interface Real {
  kind: DatumKind.Real,
  value: number;
}

export interface String {
  kind: DatumKind.String,
  value: string,
}

export interface Vector {
  kind: DatumKind.Vector,
  value: Array<Datum>,
}

export interface Hash {
  kind: DatumKind.Hash,
  value: Map<Datum, Datum>,
}

export interface Port {
  kind: DatumKind.Port,
  value: ReadPort | WritePort | ReadWritePort;
}

export interface Pair {
  kind: DatumKind.Pair,
  left: Datum,
  right: Datum,
}

export class Environment {
  readonly kind = DatumKind.Environment;

  constructor(private env = new Map<string, Datum>()) { }

  read(name: string): Datum {
    if (!this.env.has(name)) {
      throw new RuntimeError(`undefined variable '${name}'`);
    }

    return this.env.get(name)!;
  }

  write(name: string, datum: Datum): Datum {
    this.env.set(name, datum);
    return datum;
  }

  modify(name: string, datum: Datum): Datum {
    if (!this.env.has(name)) {
      throw new RuntimeError(`undefined variable '${name}'`);
    }

    this.env.set(name, datum);
    return datum;
  }

  extend(vars: [string, Datum][]): Environment {
    let newEnv = new Map(this.env);

    for (const [name, datum] of vars) {
      newEnv.set(name, datum);
    }

    return new Environment(newEnv);
  }
}

export interface JSFunction {
  kind: DatumKind.JSFunction,
  value: (env: Interpreter, args: Array<Datum>) => Datum,
}

export interface Procedure {
  kind: DatumKind.Procedure,
  body: Array<Datum>,
  varParam: string | null,
  parameters: Array<string>,
  closure: Environment,
}

/**
 * Check if a value is truthy or falsy, only false booleans
 * are falsy.
 */
export function isTruthy(datum: Datum): boolean {
  return !(datum.kind == DatumKind.Boolean && datum.value == false);
}

export function tryCast<T extends Datum>(datum: Datum, kind: T['kind']): T | null {
  if (kind === datum.kind) {
    return datum as T;
  } else {
    return null;
  }
}

export function cast<T extends Datum>(datum: Datum, kind: T['kind']): T {
  if (kind === datum.kind) {
    return datum as T;
  } else {
    throw new RuntimeError(`unable to cast '${datum}'`);
  }
}

export function pair(data: Datum[]): Datum {
  return data.reduceRight(
    (right, left) => ({ kind: DatumKind.Pair, left, right }),
    { kind: DatumKind.Symbol, value: '()' }
  );
}

export function unpair(datum: Datum): Datum[] {
  const accum = [];

  while (datum.kind === DatumKind.Pair) {
    accum.push(datum.left);
    datum = datum.right;
  }

  if (datum.kind !== DatumKind.Symbol || datum.value !== '()') {
    accum.push(datum);
  }

  return accum;
}

export function mkVoid(): Void {
  return Object.freeze({
    kind: DatumKind.Void
  });
}

export function mkNil(): Symbol {
  return Object.freeze({
    kind: DatumKind.Symbol,
    value: '()',
  });
}

export function mkSymbol(value: string): Symbol {
  return Object.freeze({
    value,
    kind: DatumKind.Symbol,
  });
}

export function mkBoolean(value: boolean): Boolean {
  return Object.freeze({
    value,
    kind: DatumKind.Boolean,
  });
}

export function mkCharacter(value: number | string): Character {
  return Object.freeze({
    kind: DatumKind.Character,
    value: typeof value == 'number' ? value : value.charCodeAt(0),
  });
}

export function mkInteger(value: number): Integer {
  return Object.freeze({
    kind: DatumKind.Integer,
    value: Math.floor(value),
  });
}

export function mkReal(value: number): Real {
  return Object.freeze({
    value,
    kind: DatumKind.Real,
  });
}

export function mkString(value: string): String {
  return Object.freeze({
    value: value,
    kind: DatumKind.String,
  });
}

export function mkVector(...values: Datum[]): Vector {
  return {
    kind: DatumKind.Vector,
    value: values,
  };
}

export function mkHash(...values: [Datum, Datum][]): Hash {
  return {
    kind: DatumKind.Hash,
    value: new Map(values),
  };
}

export function mkPort(value: ReadPort | WritePort | ReadWritePort): Port {
  return {
    value,
    kind: DatumKind.Port,
  };
}

export function mkPair(left: Datum, right: Datum): Pair {
  return Object.freeze({
    left, right,
    kind: DatumKind.Pair,
  });
}

export function mkJSFunction(
  value: (env: Interpreter, args: Array<Datum>) => Datum
): JSFunction {
  return Object.freeze({
    value,
    kind: DatumKind.JSFunction,
  });
}

export function mkProcedure(
  body: Array<Datum>,
  varParam: string | null,
  parameters: Array<string>,
  closure: Environment,
): Procedure {
  return Object.freeze({
    body, varParam, parameters, closure,
    kind: DatumKind.Procedure,
  });
}

export function toString(datum: Datum): string {
  switch (datum.kind) {
    case DatumKind.Void:
      return '#<void>';

    case DatumKind.Symbol:
      return datum.value;

    case DatumKind.Boolean:
      return datum.value ? '#t' : '#f';

    case DatumKind.Character:
      return `#'${String.fromCharCode(datum.value)}`;

    case DatumKind.Integer:
    case DatumKind.Real:
      return `${datum.value}`;

    case DatumKind.String:
      return `"${datum.value}"`;

    case DatumKind.Hash:
      return '#<hash>';

    case DatumKind.Port:
      return '#<port>';

    case DatumKind.Pair:
      return `(${unpair(datum).map(toString).join(' ')})`;

    case DatumKind.Vector:
      return `#(${datum.value.map(toString).join(' ')})`;

    case DatumKind.Environment:
      return '#<environement>';

    case DatumKind.JSFunction:
      return '#<js-function>';

    case DatumKind.Procedure:
      return '#<procedure>';
  }
}
