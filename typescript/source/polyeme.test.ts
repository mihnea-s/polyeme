import { Datum, DatumKind } from './datum';
import { isTruthy } from './polyeme';

test('test isTruthy', () => {
  const mappings: [Datum, boolean][] = [
    [{ kind: DatumKind.Boolean, value: false }, false],
    [{ kind: DatumKind.Boolean, value: true }, true],
  ];

  for (const [val, want] of mappings) {
    expect(isTruthy(val)).toBe(want);
  }
});
