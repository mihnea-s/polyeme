import {
  Datum,
  DatumKind,
  mkBoolean,
  mkCharacter,
  mkInteger,
  mkNil,
  mkPair,
  mkReal,
  mkString,
  mkSymbol,
  mkVector
} from './datum';

export class ParsingError {
  constructor(
    public description: string,
    public location: [number, number],
  ) { }
};

export class Parser {
  private buffer: string = '';
  private location: [number, number] = [0, 0];

  /**
   * Check if the buffer reached its end.
   */
  private eof(): boolean {
    return this.buffer.length == 0;
  }

  /**
   * Skip ahead in the buffer `n` characters.
   */
  private forward(n: number = 1): void {
    this.buffer = this.buffer.substring(n).trimStart();
  }

  /**
   * Check if the buffer starts with one of `args`
   * and skip ahead if it does.
   */
  private follows(...args: string[]): boolean {
    for (const prefix of args) {
      if (this.buffer.startsWith(prefix)) {
        this.forward(prefix.length);
        return true;
      }
    }

    return false;
  }

  /**
   * Check if the buffer starts with one of `args`,
   * throws an error if it doesn't.
   */
  private expect(...args: string[]): void {
    if (!this.follows(...args)) {
      const expected = args.map(a => `'${a}'`).join(', ');
      const actual = this.buffer.substr(0, 10);

      throw new ParsingError(
        `expected one of ${expected}, but found '${actual}..'`,
        this.location,
      );
    }
  }

  private expression(): Datum {
    this.buffer = this.buffer.trimLeft();

    // Handle booleans
    if (this.follows('#t')) {
      return mkBoolean(true);
    } else if (this.follows('#f')) {
      return mkBoolean(false);
    }

    // Handle quoting
    if (this.follows('\'')) {
      const expr = this.expression();
      return mkPair(mkSymbol('quote'), expr);
    }

    // Handle vector literals
    if (this.follows('#(')) {
      let vec = new Array();

      while (!this.follows(')')) {
        vec.push(this.expression());
      }

      return mkVector(...vec);
    }

    let matches: RegExpExecArray | null;

    // match and parse real number literals (with comma)
    if (matches = /^(\d+),(\d+)/.exec(this.buffer)) {
      this.forward(matches[0].length);
      return mkReal(Number.parseFloat(matches.slice(1).join('.')));
    }

    // match and parse hex integer literals
    if (matches = /^(0x[\da-f]+)/.exec(this.buffer)) {
      this.forward(matches[0].length);
      return mkInteger(Number.parseInt(matches[1]));
    }

    // match and parse integer literals (including binary)
    if (matches = /^((?:0b)?[\d]+)/.exec(this.buffer)) {
      this.forward(matches[0].length);

      let value = 0;

      if (matches[1].startsWith('0b')) {
        const str = matches[1].substring('0b'.length);
        value = Number.parseInt(str, 2);
      } else {
        value = Number.parseInt(matches[1]);
      }

      return mkInteger(value);
    }

    // match and capture character literals
    if (matches = /^#'(\S)/.exec(this.buffer)) {
      this.forward(matches[0].length);
      return mkCharacter(matches[1].codePointAt(0)!);
    }

    // match and capture string literals
    if (matches = /^"([^"]*?(?:\\"[^"]*?)*?)"/.exec(this.buffer)) {
      this.forward(matches[0].length);
      return mkString(matches[1].toString());
    }

    // symbol regex
    const symbolRegex = /^[-!#$%&|*+/:<=>?@^_~\p{Ll}][-!#$%&|*+/:<=>?@^_~\d\p{Ll}]*/u;

    // match and capture valid symbols
    if (matches = symbolRegex.exec(this.buffer)) {
      const symbol = matches[0].toString();
      this.forward(symbol.length);
      return mkSymbol(symbol);
    }

    // Match and capture possible var args
    if (matches = /^\.\./.exec(this.buffer)) {
      this.forward(matches[0].length);
      return mkSymbol('..');
    }

    this.expect('(', '[');

    if (this.follows(')', ']')) {
      return mkNil();
    }

    let root: Datum = {
      kind: DatumKind.Pair,
      left: this.expression(),
      right: {
        kind: DatumKind.Symbol,
        value: '()',
      },
    };

    // right associative fold of paranthesised lists
    for (let pair = root; !this.follows(')', ']'); pair = pair.right) {
      pair.right = {
        kind: DatumKind.Pair,
        left: this.expression(),
        right: pair.right,
      };
    }

    return root;
  }

  parseLine(line: string): Datum | ParsingError {
    this.buffer = line.trimStart();

    let expr;

    try {
      expr = this.expression();
    } catch (error) {
      return error;
    }

    if (!this.eof()) return new ParsingError(
      'expected end of line',
      this.location,
    );

    return expr;
  }

  parseLines(lines: string): Datum[] | ParsingError[] {
    this.buffer = lines.trimStart();

    let exprs = [];
    let errors = [];

    while (!this.eof()) {
      try {
        exprs.push(this.expression());
      } catch (error) {
        errors.push(error);
        this.forward(this.buffer.indexOf('\n'));
      }
    };

    if (!this.eof()) return [
      ...errors,
      new ParsingError(
        'expected end of file',
        this.location,
      )
    ];

    if (errors.length > 0) {
      return errors;
    }

    return exprs;
  }
}

