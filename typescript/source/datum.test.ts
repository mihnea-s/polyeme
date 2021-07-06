import {
  Real,
  Environment,
  Datum,
  DatumKind,
  isTruthy,
} from './datum';

test('test Environment', () => {
  const testDatum: Real = {
    kind: DatumKind.Real,
    value: 2.0,
  };

  let testEnv = new Environment();

  expect(testEnv.write('x', testDatum)).toEqual(testDatum);
  expect(testEnv.read('x')).toEqual(testDatum);

  const extendDatums: [string, Datum][] = [
    [
      'y', {
        kind: DatumKind.String,
        value: 'test',
      }
    ],
    [
      'x', {
        kind: DatumKind.Hash,
        value: new Map(),
      }
    ],
  ];

  // test env.extend
  const newEnv = testEnv.extend(extendDatums);

  // old env doesnt have new values
  expect(() => testEnv.read('y')).toThrow();
  expect(testEnv.read('x')).toEqual(testDatum);

  // new env is not equal to old env
  expect(newEnv).not.toEqual(testEnv);

  // all values are in new env
  for (const [name, datum] of extendDatums) {
    expect(newEnv.read(name)).toEqual(datum);
  }

  // undefined value thows
  expect(() => newEnv.read('z')).toThrow();
});

test('test isTruthy', () => {
  const mappings: [Datum, boolean][] = [
    [{ kind: DatumKind.Boolean, value: false }, false],
    [{ kind: DatumKind.Boolean, value: true }, true],
    [{ kind: DatumKind.Character, value: 2 }, true],
  ];

  for (const [val, want] of mappings) {
    expect(isTruthy(val)).toBe(want);
  }
});
