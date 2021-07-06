import { DatumKind } from './datum';
import { Parser } from './parser';

test('test parser', () => {
  const parser = new Parser();

  expect(parser.parseLine('#(1 0b10 0xf0 2,6)')).toEqual({
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
  });

  expect(parser.parseLine('(quote a b)')).toEqual({
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
  });
});