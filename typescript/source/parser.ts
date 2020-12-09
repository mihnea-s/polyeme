import {Datum, DatumKind} from './datum';

export class Parser {
  private buffer: string = '';

  private eof(): boolean {
    return this.buffer.length == 0;
  }

  private forward(n: number = 1): void {
    this.buffer = this.buffer.substring(n);
  }

  private follows(...args: string[]): boolean {
    for (const prefix of args) {
      if (!this.buffer.startsWith(prefix)) continue;
      this.buffer = this.buffer.substring(prefix.length);
      return true;
    }

    return false;
  }

  private expect(...args: string[]): void {
    if (!this.follows(...args)) {
      throw 'TODO';
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
          kind:DatumKind.Symbol, 
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
    } else if (this.follows ('#(')) {
      let vec = new Array();

      while (!this.follows(')', ']')) {
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

      // ! TODO

      return {
        kind: DatumKind.Integer,
        value: 0,
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

  parseLine(line: string): Datum {
    this.buffer = line;

    const expr = this.expression();

    if (!this.eof()) throw 'TODO';

    return expr;
  }

  parseLines(lines: string): Datum[] {
    this.buffer = lines;

    let exprs = [];

    while (!this.eof()) exprs.push(this.expression());

    return exprs;
  }
}

