import { DatumKind } from './datum';
import { Parser } from './parser';

test('test parser', () => {
  const parser = new Parser();

  expect(parser.parseLine('#(1)')).toEqual({
    kind: DatumKind.Vector,
    value: [{
      kind: DatumKind.Integer,
      value: 1,
    }],
  });

});