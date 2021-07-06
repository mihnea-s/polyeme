import { Duplex, Writable, Readable } from 'stream';
import { RuntimeError } from './eval';

export type Datum
  = Symbol | Boolean | Character
  | Integer | Real | String
  | Vector | Hash | Port
  | Pair | Environment | JSFunction
  | Procedure;

export enum DatumKind {
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
  value: Writable | Readable | Duplex;
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
  value: (env: Environment, args: Array<Datum>) => Datum,
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
