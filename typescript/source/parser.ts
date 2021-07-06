import { Datum, DatumKind } from './datum';

type ParsingError = {
  description: string,
  location: [number, number];
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
    this.buffer = this.buffer.substring(n);
  }

  /**
   * Check if the buffer starts with one of `args`
   * and skip ahead if it does.
   */
  private follows(...args: string[]): boolean {
    for (const prefix of args) {
      if (!this.buffer.startsWith(prefix)) continue;
      this.buffer = this.buffer.substring(prefix.length);
      return true;
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

      throw {
        location: this.location,
        description: `expected one of ${expected}, but found none`
      };
    }
  }

  private expression(): Datum {
    this.buffer = this.buffer.trimLeft();

    // handle quoting
    if (this.follows('\'')) {
      const expr = this.expression();
      return {
        kind: DatumKind.Pair,
        right: expr,
        left: {
          kind: DatumKind.Symbol,
          value: 'quote'
        },
      };
    }

    // handle hash literals (bool & vec)
    if (this.follows('#t')) {
      return {
        kind: DatumKind.Boolean,
        value: false,
      };
    } else if (this.follows('#f')) {
      return {
        kind: DatumKind.Boolean,
        value: false,
      };
    } else if (this.follows('#(')) {
      let vec = new Array();

      while (!this.follows(')')) {
        vec.push(this.expression());
      }

      return {
        kind: DatumKind.Vector,
        value: vec,
      };
    }

    let matches: RegExpExecArray | null;

    // match and parse real number literals (with comma)
    if (matches = /^(\d+),(\d+)/.exec(this.buffer)) {
      this.forward(matches[0].length);

      return {
        kind: DatumKind.Real,
        value: Number.parseFloat(matches.slice(1).join('.')),
      };
    }

    // match and parse integer literals (including hex and bin)
    if (matches = /^((?:0x|b)?[\da-f]+)/.exec(this.buffer)) {
      this.forward(matches[0].length);

      let value = 0;

      if (matches[1].startsWith('0b')) {
        const str = matches[1].substring('0b'.length);
        value = Number.parseInt(str, 2);
      } else {
        value = Number.parseInt(matches[1]);
      }

      return {
        kind: DatumKind.Integer,
        value: value,
      };
    }

    // match and capture character literals
    if (matches = /^#'(\X)/.exec(this.buffer)) {
      this.forward(matches[0].length);

      return {
        kind: DatumKind.Character,
        value: matches[1].codePointAt(0)!,
      };
    }

    // match and capture string literals
    if (matches = /^"([^"]*?(?:\\"[^"]*?)*?)"/.exec(this.buffer)) {
      this.forward(matches[0].length);

      return {
        kind: DatumKind.String,
        value: matches[1].toString(),
      };
    }

    // symbol regex (split over multiple lines)
    const symbolRegex = new RegExp([
      /^[-!#$%&|*+/:<=>?@^_~\p{Ll}]/,
      /[-!#$%&|*+/:<=>?@^_~\d\p{Ll}]*/,
    ].map(r => r.source).join(''));

    // match and capture valid symbols
    if (matches = symbolRegex.exec(this.buffer)) {
      const symbol = matches[0].toString();
      this.forward(symbol.length);

      return {
        kind: DatumKind.Symbol,
        value: symbol,
      };
    }

    this.expect('(', '[');

    let pair: Datum = {
      kind: DatumKind.Symbol,
      value: '()'
    };

    while (!this.follows(')', ']')) {
      pair = {
        kind: DatumKind.Pair,
        left: this.expression(),
        right: pair,
      };
    }

    return pair;
  }

  parseLine(line: string): Datum | ParsingError {
    this.buffer = line;

    let expr;

    try {
      expr = this.expression();
    } catch (error) {
      return error;
    }

    if (!this.eof()) return {
      location: this.location,
      description: 'expected end of line',
    };

    return expr;
  }

  parseLines(lines: string): Datum[] | ParsingError[] {
    this.buffer = lines;

    let exprs = [];
    let errors = [];

    while (!this.eof()) {
      try {
        exprs.push(this.expression());
      } catch (error) {
        errors.push(error);
      }
    };

    if (!this.eof()) return [...errors, {
      location: this.location,
      description: 'expected end of file',
    }];

    if (errors.length > 0) {
      return errors;
    }

    return exprs;
  }
}

