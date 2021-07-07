import { Interpreter, RuntimeError } from './eval';
import { Parser, ParsingError } from './parser';

import {
  ReadPort,
  WritePort,
  getStdin,
  getStdout,
  systemExec,
  readFileToString,
  openFileWrite,
  openFileRead,
  abortExecution
} from './platform';

import {
  Boolean,
  cast,
  Character,
  Datum,
  DatumKind,
  Environment,
  Integer,
  mkBoolean,
  mkInteger,
  mkPair,
  mkPort,
  mkReal,
  mkString,
  mkVector,
  mkVoid,
  Pair,
  pair,
  Port,
  Real,
  String,
  Symbol,
  toString,
  Vector,
} from './datum';

export const Preloaded: [string, (i: Interpreter, a: Datum[]) => Datum][] = [
  /**************/
  // Primitives //
  /**************/
  ['set!', setFn],
  ['exit', exitFn],
  ['load', loadFn],
  ['eval', evalFn],
  ['apply', applyFn],
  ['system', systemFn],
  ['void', voidFn],

  /*********************/
  // Boolean functions //
  /*********************/
  ['and', opBoolBoolToBool((a, b) => a && b)],
  ['or', opBoolBoolToBool((a, b) => a || b)],

  /***********************/
  // Character functions //
  /***********************/
  ['chr=?', opChrChrToBool((a, b) => a == b)],
  ['chr>?', opChrChrToBool((a, b) => a > b)],
  ['chr<?', opChrChrToBool((a, b) => a < b)],
  ['chr>=?', opChrChrToBool((a, b) => a >= b)],
  ['chr<=?', opChrChrToBool((a, b) => a <= b)],
  ['chr-digit?', opChrToBool(isDigit)],
  ['chr-alpha?', opChrToBool(isAlpha)],
  ['chr-alnum?', opChrToBool(isAlphaNum)],
  ['chr-space?', opChrToBool(isSpace)],
  ['chr-lower?', opChrToBool(isLower)],
  ['chr-upper?', opChrToBool(isUpper)],

  /*********************/
  // Numeric functions //
  /*********************/
  ['-', opNumNumToNum((a, b) => a - b)],
  ['+', opNumNumToNum((a, b) => a + b)],
  ['*', opNumNumToNum((a, b) => a * b)],
  ['/', opNumNumToNum((a, b) => a / b)],
  ['//', opNumNumToNum((a, b) => Math.floor(a / b))],
  ['mod', opNumNumToNum((a, b) => a % b)],
  ['=?', opNumNumToBool((a, b) => a == b)],
  ['<?', opNumNumToBool((a, b) => a < b)],
  ['>?', opNumNumToBool((a, b) => a > b)],
  ['/=?', opNumNumToBool((a, b) => a != b)],
  ['>=?', opNumNumToBool((a, b) => a >= b)],
  ['<=?', opNumNumToBool((a, b) => a <= b)],
  ['ceil', opRealToReal(Math.ceil)],
  ['floor', opRealToReal(Math.floor)],
  ['trunc', opRealToReal(Math.trunc)],
  ['round', opRealToReal(Math.round)],
  ['exp', opRealToReal(Math.exp)],
  ['log', opRealToReal(Math.log)],
  ['sin', opRealToReal(Math.sin)],
  ['cos', opRealToReal(Math.cos)],
  ['tan', opRealToReal(Math.tan)],
  ['asin', opRealToReal(Math.asin)],
  ['acos', opRealToReal(Math.acos)],
  ['atan', opRealToReal(Math.atan)],

  /********************/
  // String functions //
  /********************/
  ['str', strEmptyProc],
  ['str@', strAtProc],
  ['str+', strAppendProc],
  ['str-nl', strNewlineProc],
  ['str-len', strLenProc],
  ['str-sub', strSubstringProc],
  ['str-set!', strSetProc],
  ['str-fill!', strFillProc],
  ['str=?', opStrStrToBool((a, b) => a == b)],
  ['str<?', opStrStrToBool((a, b) => a < b)],
  ['str>?', opStrStrToBool((a, b) => a > b)],
  ['str<=?', opStrStrToBool((a, b) => a <= b)],
  ['str>=?', opStrStrToBool((a, b) => a >= b)],

  /********************/
  // Vector functions //
  /********************/
  ['vec', vecEmptyProc],
  ['vec@', vecAtProc],
  ['vec-len', vecLenProc],
  ['vec-set!', vecSetProc],
  ['vec-fill!', vecFillProc],

  /******************/
  // Hash functions //
  /******************/
  ['hash', hashEmptyProc],
  ['hash@', hashAtProc],
  ['hash-set!', hashSetProc],
  ['hash-keys', hashKeysProc],
  ['hash-vals', hashValsProc],

  /******************/
  // Pair functions //
  /******************/
  ['car', carProc],
  ['cdr', cdrProc],
  ['cons', consProc],
  ['nil?', isNilProc],
  ['list', listProc],

  /****************/
  // IO functions //
  /****************/
  ['read', readProc],
  ['write', writeProc],
  ['open-input-file', openInputFile],
  ['open-output-file', openOutputFile],
  ['close-input-port', closePort],
  ['close-output-port', closePort],
  ['read-contents', readContents],

  /*************************/
  // Polymorphic functions //
  /*************************/
  ['eq?', areEqual],
  ['same?', areSame],
  ['sym?', isOfDatumKind(DatumKind.Symbol)],
  ['bool?', isOfDatumKind(DatumKind.Boolean)],
  ['chr?', isOfDatumKind(DatumKind.Character)],
  ['int?', isOfDatumKind(DatumKind.Integer)],
  ['real?', isOfDatumKind(DatumKind.Real)],
  ['str?', isOfDatumKind(DatumKind.String)],
  ['vec?', isOfDatumKind(DatumKind.Vector)],
  ['hash?', isOfDatumKind(DatumKind.Hash)],
  ['port?', isOfDatumKind(DatumKind.Port)],
  ['list?', isOfDatumKind(DatumKind.Pair)],
  ['env?', isOfDatumKind(DatumKind.Environment)],
  ['proc?', isOfDatumKind(DatumKind.Procedure)],

  /**************************/
  // Type casting functions //
  /**************************/
  ['sym->str', symToStrProc],
  ['bool->str', boolToStrProc],
  ['chr->str', chrToStrProc],
  ['chr->int', chrToIntProc],
  ['int->chr', intToChrProc],
  ['num->str', numToStrProc],
  ['str->sym', strToSymProc],
  ['str->int', strToIntProc],
  ['str->real', strToRealProc],
  ['str->vec', strToVecProc],
  ['vec->str', vecToStrProc],
  ['vec->list', vecToListProc],
  ['hash->vec', hashToVecProc],
  ['hash->env', hashToEnvProc]
];

type DatumKinds<T extends [...Datum[]]> = { length: T['length'] } & {
  [I in keyof T]: T[I] extends Datum ? T[I]['kind'] | null : never
};

function noArguments(args: Datum[]) {
  if (args.length > 0) {
    throw new RuntimeError(
      `function called with ${args.length} argument(s):`
      + `${args.map(toString).join(', ')} but expected none`
    );
  }
}

function minArguments(args: Datum[], count: number) {
  if (args.length <= count) {
    throw new RuntimeError(
      `function called with ${args.length} argument(s) but`
      + ` needs at least ${count}`
    );
  }
}

function readArguments<T extends [...Datum[]]>(args: Datum[], kinds: DatumKinds<T>): T {
  if (args.length !== kinds.length) {
    throw new RuntimeError(
      `function called with ${args.length} argument(s)`
      + ` instead of ${kinds.length}`
    );
  }

  args.forEach((arg, i) => {
    if (kinds[i] && kinds[i] != arg.kind) {
      throw new RuntimeError(
        `argument '${toString(arg)}' (${i}) is of type ${arg.kind}, `
        + ` expected type ${kinds[i]}`
      );
    }
  });

  return args as T;
}

/**************/
// Primitives //
/**************/

function setFn(interp: Interpreter, args: Datum[]): Datum {
  const [name, value] = readArguments<[Symbol, Datum]>(args, [
    DatumKind.Symbol, null
  ]);

  return interp.environment().modify(name.value, value);
}

function exitFn(_: Interpreter, args: Datum[]): Datum {
  getStdout().write(`Exit was called ${args.map(toString).join(', ')}\n`);
  abortExecution();
}

function loadFn(interp: Interpreter, args: Datum[]): Datum {
  const [file] = readArguments<[String]>(args, [DatumKind.String]);

  const content = readFileToString(file.value);

  const data = new Parser()
    .parseLines(content)
    // @ts-ignore
    .map((result: Datum | ParsingError) => {
      if (result instanceof ParsingError) {
        throw result;
      } else {
        return interp.tryEvaluate(result);
      }
    });

  return pair(data);
}

function evalFn(interp: Interpreter, args: Datum[]): Datum {
  const [arg, env] = readArguments<[Datum, Environment]>(args, [
    null, DatumKind.Environment
  ]);

  const evaled = interp.evaluateWith(arg, env);

  if (evaled instanceof RuntimeError) {
    throw evaled;
  } else {
    return evaled;
  }
}

function applyFn(interp: Interpreter, args: Datum[]): Datum {
  minArguments(args, 1);
  const [fn, fnargs] = [args[0], args.slice(1)];
  return interp.tryEvaluate(mkPair(fn, pair(fnargs)));
}

function systemFn(_: Interpreter, args: Datum[]): Datum {
  minArguments(args, 1);

  const cmd = cast<String>(args[0], DatumKind.String).value;

  const options = args
    .slice(1)
    .map(d => cast<String>(d, DatumKind.String).value);

  const [stdin, stdout] = systemExec(cmd, options);
  return mkPair(mkPort(stdin), mkPort(stdout));
}

function voidFn(_: Interpreter, args: Datum[]): Datum {
  noArguments(args);
  return mkVoid();
}

/*********************/
// Boolean functions //
/*********************/

function opBoolBoolToBool(op: (a: boolean, b: boolean) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [a, b] = readArguments<[Boolean, Boolean]>(
      args, [DatumKind.Boolean, DatumKind.Boolean]
    );

    return mkBoolean(op(a.value, b.value));
  };
}

/***********************/
// Character functions //
/***********************/

function isDigit(a: number): boolean {
  return '0'.charCodeAt(0) <= a && a <= '9'.charCodeAt(0);
}

function isAlpha(a: number): boolean {
  return !!String.fromCharCode(a).match(/(\p{Ll}|\p{Lu})/u);
}

function isAlphaNum(a: number): boolean {
  return isDigit(a) || isAlpha(a);
}

function isSpace(a: number): boolean {
  const str = String.fromCharCode(a);
  return str.trim() === str;
}

function isLower(a: number): boolean {
  const str = String.fromCharCode(a);
  return str.toLowerCase() === str;
}

function isUpper(a: number): boolean {
  const str = String.fromCharCode(a);
  return str.toUpperCase() === str;
}

function opChrChrToBool(op: (a: number, b: number) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [a, b] = readArguments<[Character, Character]>(
      args, [DatumKind.Character, DatumKind.Character]
    );

    return mkBoolean(op(a.value, b.value));
  };
}

function opChrToBool(op: (a: number) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [a] = readArguments<[Character]>(args, [DatumKind.Character]);
    return mkBoolean(op(a.value));
  };
}

/*********************/
// Numeric functions //
/*********************/

function opNumNumToNum(op: (a: number, b: number) => number) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [a, b] = readArguments<[Real | Integer, Real | Integer]>(
      args, [null, null]
    );

    return mkReal(op(a.value, b.value));
  };
}

function opNumNumToBool(op: (a: number, b: number) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [a, b] = readArguments<[Real | Integer, Real | Integer]>(
      args, [null, null]
    );

    return mkBoolean(op(a.value, b.value));
  };
}


function opRealToReal(op: (x: number) => number) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [x] = readArguments<[Real]>(args, [DatumKind.Real]);
    return mkReal(op(x.value));
  };
}

/********************/
// String functions //
/********************/

function strEmptyProc(_: Interpreter, args: Datum[]): Datum {
  noArguments(args);
  return mkString('');
}

function strAtProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strAppendProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strNewlineProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strLenProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strSubstringProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strSetProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strFillProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function opStrStrToBool(op: (a: string, b: string) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [a, b] = readArguments<[String, String]>(args, [
      DatumKind.String, DatumKind.String
    ]);

    return mkBoolean(op(a.value, b.value));
  };
}


/********************/
// Vector functions //
/********************/

function vecEmptyProc(_: Interpreter, args: Datum[]): Datum {
  noArguments(args);
  return mkVector();
}

function vecAtProc(_: Interpreter, args: Datum[]): Datum {
  const [vec, idx] = readArguments<[Vector, Integer]>(args, [
    DatumKind.Vector, DatumKind.Integer,
  ]);

  if (idx.value >= vec.value.length) {
    throw new RuntimeError(
      `index '${idx.value}' out of bounds of vector`
      + ` with ${vec.value.length} elements`
    );
  }

  return vec.value[idx.value];
}

function vecLenProc(_: Interpreter, args: Datum[]): Datum {
  const [vec] = readArguments<[Vector]>(args, [DatumKind.Vector]);
  return mkInteger(vec.value.length);
}

function vecSetProc(_: Interpreter, args: Datum[]): Datum {
  const [vec, idx, value] = readArguments<[Vector, Integer, Datum]>(args, [
    DatumKind.Vector, DatumKind.Integer, null
  ]);

  if (idx.value >= vec.value.length) {
    throw new RuntimeError(
      `index '${idx.value}' out of bounds of vector`
      + ` with ${vec.value.length} elements`
    );
  }

  return (vec.value[idx.value] = value);
}

function vecFillProc(_: Interpreter, args: Datum[]): Datum {
  const [vec, value] = readArguments<[Vector, Datum]>(args, [
    DatumKind.Vector, null
  ]);

  vec.value.fill(value);
  return mkVoid();
}

/******************/
// Hash functions //
/******************/

function hashEmptyProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function hashAtProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function hashSetProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function hashKeysProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function hashValsProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

/******************/
// Pair functions //
/******************/

function carProc(_: Interpreter, args: Datum[]): Datum {
  const [x] = readArguments<[Pair]>(args, [DatumKind.Pair]);
  return x.left;
};

function cdrProc(_: Interpreter, args: Datum[]): Datum {
  const [x] = readArguments<[Pair]>(args, [DatumKind.Pair]);
  return x.right;
}

function consProc(_: Interpreter, args: Datum[]): Datum {
  const [car, cdr] = readArguments<[Datum, Datum]>(args, [null, null]);
  return mkPair(car, cdr);
}

function isNilProc(_: Interpreter, args: Datum[]): Datum {
  const [x] = readArguments<[Datum]>(args, [null]);
  return mkBoolean(x.kind === DatumKind.Symbol && x.value === '()');
}

function listProc(_: Interpreter, args: Datum[]): Datum {
  return pair(args);
}

/*************************/
// Polymorphic functions //
/*************************/

function areEqual(_: Interpreter, args: Datum[]): Datum {
  const [a, b] = readArguments<[Datum, Datum]>(args, [null, null]);
  return mkBoolean(JSON.stringify(a) === JSON.stringify(b));
}

function areSame(_: Interpreter, args: Datum[]): Datum {
  const [a, b] = readArguments<[Datum, Datum]>(args, [null, null]);
  return mkBoolean(a === b);
}

function isOfDatumKind(kind: DatumKind) {
  return (_: Interpreter, args: Datum[]): Datum => {
    const [arg] = readArguments<[Datum]>(args, [null]);
    return mkBoolean(arg.kind === kind);
  };
}

/****************/
// IO functions //
/****************/

function readProc(_: Interpreter, args: Datum[]): Datum {
  switch (args.length) {
    case 0: {
      return mkString(getStdin().read());
    }

    default: {
      const [port] = readArguments<[Port]>(args, [DatumKind.Port]);

      if (port.value instanceof WritePort) {
        throw new RuntimeError('port is write-only');
      }

      return mkString(port.value.read());
    }
  }
}

function writeProc(_: Interpreter, args: Datum[]): Datum {
  switch (args.length) {
    case 1: {
      const [str] = readArguments<[String]>(args, [DatumKind.String]);
      getStdout().write(str.value);
      return mkVoid();
    }

    default: {
      const [port, str] = readArguments<[Port, String]>(args, [
        DatumKind.Port, DatumKind.String
      ]);

      if (port.value instanceof ReadPort) {
        throw new RuntimeError('port is read-only');
      }

      port.value.write(str);
      return mkVoid();
    }
  }
}

function openInputFile(_: Interpreter, args: Datum[]): Datum {
  const [file] = readArguments<[String]>(args, [DatumKind.String]);
  return mkPort(openFileRead(file.value));
}

function openOutputFile(_: Interpreter, args: Datum[]): Datum {
  const [file] = readArguments<[String]>(args, [DatumKind.String]);
  return mkPort(openFileWrite(file.value));
}

function closePort(_: Interpreter, args: Datum[]): Datum {
  const [port] = readArguments<[Port]>(args, [DatumKind.Port]);
  port.value.destroy();
  return mkVoid();
}

function readContents(_: Interpreter, args: Datum[]): Datum {
  const [input] = readArguments<[String | Port]>(args, [null]);

  switch (input.kind) {
    case DatumKind.String: {
      return mkString(readFileToString(input.value));
    }

    case DatumKind.Port: {
      if (input.value instanceof WritePort) {
        throw new RuntimeError('port is write-only');
      }

      return mkString(input.value.read());
    }
  }
}

/**************************/
// Type casting functions //
/**************************/

function symToStrProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function boolToStrProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function chrToStrProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function chrToIntProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function intToChrProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function numToStrProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strToSymProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strToIntProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strToRealProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function strToVecProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function vecToStrProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function vecToListProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function hashToVecProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}

function hashToEnvProc(_: Interpreter, args: Datum[]): Datum {
  return mkVoid();
}
