import {Real, Environment, Datum, DatumKind} from './datum';

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
