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
  openFileRead
} from './platform';

import {
  Boolean,
  cast,
  Character,
  Datum,
  DatumKind,
  Environment,
  mkVoid,
  pair,
  Port,
  String,
  Symbol,
  tryCast,
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
  // ("-", opNumNumNum(-)),
  // ("+", opNumNumNum(+)),
  // ("*", opNumNumNum(*)),
  // ("/", numDivision AsReal),
  // ("//", numDivision AsInt),
  // ("mod", numDivision AsReminder),
  // ("=?", opNumNumBool(==)),
  // ("<?", opNumNumBool(<)),
  // (">?", opNumNumBool(>)),
  // ("/=?", opNumNumBool(/=)),
  // (">=?", opNumNumBool(>=)),
  // ("<=?", opNumNumBool(<=)),
  // ("ceil", opRealReal(ceiling)),
  // ("floor", opRealReal(floor)),
  // ("trunc", opRealReal(truncate)),
  // ("round", opRealReal(round)),
  // ("exp", opRealReal(exp)),
  // ("log", opRealReal(log)),
  // ("sin", opRealReal(sin)),
  // ("cos", opRealReal(cos)),
  // ("tan", opRealReal(tan)),
  // ("asin", opRealReal(asin)),
  // ("acos", opRealReal(acos)),
  // ("atan", opRealReal(atan)),

  /********************/
  // String functions //
  /********************/
  // ("str", makeDatum(String) ""),
  // ("str@", containerAt StrCtner),
  // ("str+", opStrStrStr(++)),
  // ("str-nl", strStringNewline),
  // ("str-len", containerLen StrCtner),
  // ("str-sub", strSubstring),
  // ("str-set!", containerSet StrCtner),
  // ("str-fill!", containerFill StrCtner),
  // ("str=?", opStrStrBool(==)),
  // ("str<?", opStrStrBool(<)),
  // ("str>?", opStrStrBool(>)),
  // ("str<=?", opStrStrBool(<=)),
  // ("str>=?", opStrStrBool(>=)),

  /********************/
  // Vector functions //
  /********************/
  // ("vec", makeDatum(Vector) V.empty),
  // ("vec@", containerAt VecCtner),
  // ("vec-len", containerLen VecCtner),
  // ("vec-set!", containerSet VecCtner),
  // ("vec-fill!", containerFill VecCtner),

  /******************/
  // Hash functions //
  /******************/
  // ("hash", makeDatum(Hash) H.empty),
  // ("hash@", containerAt HashCtner),
  // ("hash-set!", containerSet HashCtner),
  // ("hash-keys", hashElems(map(String).H.keys)),
  // ("hash-vals", hashElems H.elems),

  /******************/
  // Pair functions //
  /******************/
  // ("car", car),
  // ("cdr", cdr),
  // ("cons", cons),
  // ("nil?", isNil),
  // ("list", const $ return .pair),

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
  // ("eq?", isEq),
  // ("same?", isSame),
  // ("sym?", isSym),
  // ("bool?", isBool),
  // ("chr?", isChr),
  // ("int?", isInt),
  // ("real?", isReal),
  // ("str?", isStr),
  // ("vec?", isVec),
  // ("hash?", isHash),
  // ("port?", isPort),
  // ("list?", isList),
  // ("env?", isEnv),
  // ("proc?", isProc),

  /**************************/
  // Type casting functions //
  /**************************/
  // ("sym->str", unaryFunc symToStr),
  // ("bool->str", unaryFunc boolToStr),
  // ("chr->str", unaryFunc chrToStr),
  // ("chr->int", unaryFunc chrToInt),
  // ("int->chr", unaryFunc intToChr),
  // ("num->str", unaryFunc numToStr),
  // ("str->sym", unaryFunc strToSym),
  // ("str->int", unaryFunc strToInt),
  // ("str->real", unaryFunc strToReal),
  // ("str->vec", unaryFunc strToVec),
  // ("vec->str", unaryFunc vecToStr),
  // ("vec->list", unaryFunc vecToList),
  // ("hash->vec", unaryFunc hashToVec),
  // ("hash->env", unaryFunc hashToEnv)
];

/**************/
// Primitives //
/**************/

function ensureCount(args: Datum[], count: number) {
  if (args.length !== count) {
    throw new RuntimeError('function called with invalid type');
  }
}

function ensureMinCount(args: Datum[], count: number) {
  if (args.length <= count) {
    throw new RuntimeError('function called with invalid type');
  }
}

function setFn(interp: Interpreter, args: Datum[]): Datum {
  ensureCount(args, 2);
  const name = cast<Symbol>(args[0], DatumKind.Symbol);
  const value = args[1];

  interp.environment().write(name.value, value);
  return mkVoid();
}

function exitFn(_: Interpreter, args: Datum[]): Datum {
  throw `Exit was called ${args.join(', ')}`;
}

function loadFn(interp: Interpreter, args: Datum[]): Datum {
  ensureCount(args, 1);
  const file = cast<String>(args[0], DatumKind.String);
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
  ensureCount(args, 2);
  const env = cast<Environment>(args[1], DatumKind.Environment);
  const evaled = interp.evaluateWith(args[0], env);

  if (evaled instanceof RuntimeError) {
    throw evaled;
  } else {
    return evaled;
  }
}

function applyFn(interp: Interpreter, args: Datum[]): Datum {
  ensureMinCount(args, 1);

  const [fn, fnargs] = [args[0], args.slice(1)];

  return interp.tryEvaluate({
    kind: DatumKind.Pair,
    left: fn,
    right: pair(fnargs),
  });
}

function systemFn(_: Interpreter, args: Datum[]): Datum {
  ensureMinCount(args, 1);

  const cmd = cast<String>(args[0], DatumKind.String).value;

  const options = args
    .slice(1)
    .map(d => cast<String>(d, DatumKind.String).value);

  const [stdin, stdout] = systemExec(cmd, options);

  return pair([
    { kind: DatumKind.Port, value: stdin },
    { kind: DatumKind.Port, value: stdout },
  ]);
}

function voidFn(_: Interpreter, __: Datum[]): Datum {
  return mkVoid();
}

/*********************/
// Boolean functions //
/*********************/

function opBoolBoolToBool(op: (a: boolean, b: boolean) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    ensureCount(args, 2);
    const a = cast<Boolean>(args[0], DatumKind.Boolean);
    const b = cast<Boolean>(args[1], DatumKind.Boolean);

    return {
      kind: DatumKind.Boolean,
      value: op(a.value, b.value),
    };
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
    ensureCount(args, 2);
    const a = cast<Character>(args[0], DatumKind.Character);
    const b = cast<Character>(args[1], DatumKind.Character);

    return {
      kind: DatumKind.Boolean,
      value: op(a.value, b.value),
    };
  };
}

function opChrToBool(op: (a: number) => boolean) {
  return (_: Interpreter, args: Datum[]): Datum => {
    ensureCount(args, 1);
    const a = cast<Character>(args[0], DatumKind.Character);

    return {
      kind: DatumKind.Boolean,
      value: op(a.value),
    };
  };
}

/****************/
// IO functions //
/****************/

function readProc(_: Interpreter, args: Datum[]): Datum {
  if (args.length === 0) {
    return {
      kind: DatumKind.String,
      value: getStdin().read(),
    };
  }

  ensureCount(args, 1);
  const port = cast<Port>(args[0], DatumKind.Port);

  if (port.value instanceof WritePort) {
    throw new RuntimeError('port is write-only');
  }

  return {
    kind: DatumKind.String,
    value: port.value.read(),
  };
}

function writeProc(_: Interpreter, args: Datum[]): Datum {
  if (args.length === 1) {
    const str = cast<String>(args[0], DatumKind.String);
    getStdout().write(str.value);
    return mkVoid();
  }

  ensureCount(args, 2);
  const port = cast<Port>(args[2], DatumKind.Port);
  const str = cast<String>(args[1], DatumKind.String);

  if (port.value instanceof ReadPort) {
    throw new RuntimeError('port is read-only');
  }

  port.value.write(str);
  return mkVoid();
}

function openInputFile(_: Interpreter, args: Datum[]): Datum {
  ensureCount(args, 1);
  const file = cast<String>(args[0], DatumKind.String);

  return {
    kind: DatumKind.Port,
    value: openFileRead(file.value),
  };
}

function openOutputFile(_: Interpreter, args: Datum[]): Datum {
  ensureCount(args, 1);
  const file = cast<String>(args[0], DatumKind.String);

  return {
    kind: DatumKind.Port,
    value: openFileWrite(file.value),
  };
}

function closePort(_: Interpreter, args: Datum[]): Datum {
  ensureCount(args, 1);
  cast<Port>(args[0], DatumKind.Port).value.destroy();
  return mkVoid();
}

function readContents(_: Interpreter, args: Datum[]): Datum {
  ensureCount(args, 1);

  const file = tryCast<String>(args[0], DatumKind.String);

  if (file) {
    return {
      kind: DatumKind.String,
      value: readFileToString(file.value),
    };
  }

  const port = cast<Port>(args[0], DatumKind.Port);

  if (port.value instanceof WritePort) {
    throw new RuntimeError('port is write-only');
  }

  return {
    kind: DatumKind.String,
    value: port.value.read(),
  };
}