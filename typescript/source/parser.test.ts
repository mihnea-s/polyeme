import { DatumKind, Datum } from './datum';
import { Parser } from './parser';

const cases: [string, Datum][] = [
  ['#(1    0b10   \n 0xf0 \t 2,6   )', {
    kind: DatumKind.Vector,
    value: [
      {
        kind: DatumKind.Integer,
        value: 1,
      },
      {
        kind: DatumKind.Integer,
        value: 2,
      },
      {
        kind: DatumKind.Integer,
        value: 240,
      },
      {
        kind: DatumKind.Real,
        value: 2.6,
      },
    ],
  }],

  ['(quote a b)', {
    kind: DatumKind.Pair,
    left: {
      kind: DatumKind.Symbol,
      value: 'quote'
    },
    right: {
      kind: DatumKind.Pair,
      left: {
        kind: DatumKind.Symbol,
        value: 'a'
      },
      right: {
        kind: DatumKind.Pair,
        left: {
          kind: DatumKind.Symbol,
          value: 'b',
        }, right: {
          kind: DatumKind.Symbol,
          value: '()',
        },
      },
    }
  }],


  ['\'(a b)', {
    kind: DatumKind.Pair,
    left: {
      kind: DatumKind.Symbol,
      value: 'quote'
    },
    right: {
      kind: DatumKind.Pair,
      left: {
        kind: DatumKind.Symbol,
        value: 'a'
      },
      right: {
        kind: DatumKind.Pair,
        left: {
          kind: DatumKind.Symbol,
          value: 'b',
        }, right: {
          kind: DatumKind.Symbol,
          value: '()',
        },
      },
    }
  }],
];

test('test parser', () => {
  const parser = new Parser();

  for (const [source, datum] of cases) {
    expect(parser.parseLine(source)).toEqual(datum);
  }
});