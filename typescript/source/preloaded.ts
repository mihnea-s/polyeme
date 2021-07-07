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
  Hash,
  Integer,
  mkBoolean,
  mkCharacter,
  mkHash,
  mkInteger,
  mkPair,
  mkPort,
  mkReal,
  mkString,
  mkSymbol,
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
  const [str, idx] = readArguments<[String, Integer]>(args, [
    DatumKind.String, DatumKind.Integer,
  ]);

  if (idx.value >= str.value.length) {
    throw new RuntimeError(
      `index '${idx.value}' out of bounds of string`
      + ` with ${str.value.length} characters`
    );
  }

  return mkCharacter(str.value.charAt(idx.value));
}

function strAppendProc(_: Interpreter, args: Datum[]): Datum {
  const [a, b] = readArguments<[String, String]>(args, [
    DatumKind.String, DatumKind.String,
  ]);

  return mkString(a.value + b.value);
}

function strNewlineProc(_: Interpreter, args: Datum[]): Datum {
  const [str] = readArguments<[String]>(args, [DatumKind.String]);
  return mkString(str + '\n');
}

function strLenProc(_: Interpreter, args: Datum[]): Datum {
  const [str] = readArguments<[String]>(args, [DatumKind.String]);
  return mkInteger(str.value.length);
}

function strSubstringProc(_: Interpreter, args: Datum[]): Datum {
  const [str, start, end] = readArguments<[String, Integer, Integer]>(args, [
    DatumKind.String, DatumKind.Integer, DatumKind.Integer
  ]);

  if (start.value < 0 || str.value.length >= start.value) {
    throw new RuntimeError(
      `start value '${start.value}' out of bounds of string`
      + ` with ${str.value.length} characters`
    );
  }

  if (end.value < 0 || str.value.length >= end.value) {
    throw new RuntimeError(
      `end value '${end.value}' out of bounds of string`
      + ` with ${str.value.length} characters`
    );
  }

  if (start > end) {
    throw new RuntimeError(
      `substring range '${start.value}' to '${end.value}' is invalid`
    );
  }

  return mkString(str.value.substring(start.value, end.value));
}

function strSetProc(interp: Interpreter, args: Datum[]): Datum {
  const [name, idx, char] = readArguments<[Symbol, Integer, Character]>(args, [
    DatumKind.Symbol, DatumKind.Integer, DatumKind.Character
  ]);

  const str = cast<String>(
    interp.environment().read(name.value),
    DatumKind.String
  );

  if (idx.value >= str.value.length) {
    throw new RuntimeError(
      `index '${idx.value}' out of bounds of string`
      + ` with ${str.value.length} characters`
    );
  }

  const newStr = mkString(
    str.value.substring(0, idx.value)
    + String.fromCharCode(char.value)
    + str.value.substring(idx.value + 1)
  );

  interp.environment().write(name.value, newStr);

  return mkVoid();
}

function strFillProc(interp: Interpreter, args: Datum[]): Datum {
  const [name, char] = readArguments<[Symbol, Character]>(args, [
    DatumKind.Symbol, DatumKind.Character
  ]);

  const str = cast<String>(
    interp.environment().read(name.value),
    DatumKind.String
  );

  // JS's strings are immutable so we have to fake it.
  interp.environment().write(name.value, mkString(
    String.fromCharCode(char.value).repeat(str.value.length)
  ));

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

function vecSetProc(interp: Interpreter, args: Datum[]): Datum {
  const [name, idx, value] = readArguments<[Symbol, Integer, Datum]>(args, [
    DatumKind.Symbol, DatumKind.Integer, null
  ]);

  const vec = cast<Vector>(
    interp.environment().read(name.value),
    DatumKind.Vector
  );

  if (idx.value >= vec.value.length) {
    throw new RuntimeError(
      `index '${idx.value}' out of bounds of vector`
      + ` with ${vec.value.length} elements`
    );
  }

  vec.value[idx.value] = value;
  return mkVoid();
}

function vecFillProc(interp: Interpreter, args: Datum[]): Datum {
  const [name, value] = readArguments<[Symbol, Datum]>(args, [
    DatumKind.Symbol, null
  ]);

  const vec = cast<Vector>(
    interp.environment().read(name.value),
    DatumKind.Vector
  );

  vec.value.fill(value);
  return mkVoid();
}

/******************/
// Hash functions //
/******************/

function hashEmptyProc(_: Interpreter, args: Datum[]): Datum {
  return mkHash();
}

function hashAtProc(_: Interpreter, args: Datum[]): Datum {
  const [hash, key] = readArguments<[Hash, Datum]>(args, [
    DatumKind.Hash, null
  ]);

  const value = hash.value.get(hash);

  if (!value) {
    throw new RuntimeError(`hash does not contain key '${toString(key)}'`);
  }

  return value;
}

function hashSetProc(interp: Interpreter, args: Datum[]): Datum {
  const [name, key, value] = readArguments<[Symbol, Datum, Datum]>(args, [
    DatumKind.Symbol, null, null
  ]);

  // Read hash from environment
  const hash = cast<Hash>(
    interp.environment().read(name.value),
    DatumKind.Hash
  );

  hash.value.set(key, value);
  return mkVoid();
}

function hashKeysProc(_: Interpreter, args: Datum[]): Datum {
  const [hash] = readArguments<[Hash]>(args, [DatumKind.Hash]);
  return mkVector(...hash.value.keys());
}

function hashValsProc(_: Interpreter, args: Datum[]): Datum {
  const [hash] = readArguments<[Hash]>(args, [DatumKind.Hash]);
  return mkVector(...hash.value.values());
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
  const [sym] = readArguments<[Symbol]>(args, [DatumKind.Symbol]);
  return mkString(sym.value);
}

function boolToStrProc(_: Interpreter, args: Datum[]): Datum {
  const [bool] = readArguments<[Boolean]>(args, [DatumKind.Boolean]);
  return mkString(bool.value ? 'true' : 'false');
}

function chrToStrProc(_: Interpreter, args: Datum[]): Datum {
  const [char] = readArguments<[Character]>(args, [DatumKind.Character]);
  return mkString(String.fromCharCode(char.value));
}

function chrToIntProc(_: Interpreter, args: Datum[]): Datum {
  const [char] = readArguments<[Character]>(args, [DatumKind.Character]);
  return mkInteger(char.value);
}

function intToChrProc(_: Interpreter, args: Datum[]): Datum {
  const [int] = readArguments<[Integer]>(args, [DatumKind.Integer]);
  return mkCharacter(int.value);
}

function numToStrProc(_: Interpreter, args: Datum[]): Datum {
  const [num] = readArguments<[Integer | Real]>(args, [null]);
  return mkString(num.value.toString());
}

function strToSymProc(_: Interpreter, args: Datum[]): Datum {
  const [str] = readArguments<[String]>(args, [DatumKind.String]);
  return mkSymbol(str.value);
}

function strToIntProc(_: Interpreter, args: Datum[]): Datum {
  const [str] = readArguments<[String]>(args, [DatumKind.String]);
  return mkInteger(Number.parseInt(str.value));
}

function strToRealProc(_: Interpreter, args: Datum[]): Datum {
  const [str] = readArguments<[String]>(args, [DatumKind.String]);
  return mkReal(Number.parseFloat(str.value));
}

function strToVecProc(_: Interpreter, args: Datum[]): Datum {
  const [str] = readArguments<[String]>(args, [DatumKind.String]);
  return mkVector(
    ...Array.from(str.value)
      .map(c => c.charCodeAt(0))
      .map(mkCharacter)
  );
}

function vecToStrProc(_: Interpreter, args: Datum[]): Datum {
  const [vec] = readArguments<[Vector]>(args, [DatumKind.Vector]);
  return mkString(vec.value
    .map(d => cast<Character>(d, DatumKind.Character))
    .map(c => String.fromCharCode(c.value))
    .join('')
  );
}

function vecToListProc(_: Interpreter, args: Datum[]): Datum {
  const [vec] = readArguments<[Vector]>(args, [DatumKind.Vector]);
  return pair(vec.value);
}

function hashToVecProc(_: Interpreter, args: Datum[]): Datum {
  const [hash] = readArguments<[Hash]>(args, [DatumKind.Hash]);
  const vec = [] as Datum[];

  for (const [key, value] of hash.value.entries()) {
    vec.push(mkPair(key, value));
  }

  return mkVector(...vec);
}

function hashToEnvProc(_: Interpreter, args: Datum[]): Datum {
  const [hash] = readArguments<[Hash]>(args, [DatumKind.Hash]);
  const envMap = new Map<string, Datum>();

  for (const [key, value] of hash.value.entries()) {
    envMap.set(cast<String>(key, DatumKind.String).value, value);
  }

  return new Environment(envMap);
}
