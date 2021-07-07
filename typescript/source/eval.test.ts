import { Datum, DatumKind, mkReal, mkSymbol, pair } from './datum';
import { Interpreter, RuntimeError } from './eval';

const sym = mkSymbol;
const real = mkReal;

test('test Interpreter', () => {
  for (const [expr, value] of [
    [
      pair([sym('quote'), sym('a'), sym('b')]),
      pair([sym('a'), sym('b')]),
    ],
    [
      pair([pair([sym('lambda'), pair([sym('x')]), sym('x')]), real(2)]),
      real(2),
    ]
  ]) {
    const interp = new Interpreter();
    expect(interp.evaluate(expr)).toEqual(value);
  }

  for (const expr of [
    pair([sym('a'), sym('a'), sym('c')])
  ]) {
    const interp = new Interpreter();
    expect(interp.evaluate(expr)).toBeInstanceOf(RuntimeError);
  }
});
