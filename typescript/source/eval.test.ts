import { Datum, DatumKind, pair } from './datum';
import { Interpreter, RuntimeError } from './eval';

function symbol(v: string): Datum {
  return {
    kind: DatumKind.Symbol,
    value: v,
  };
}

test('test Interpreter', () => {
  for (const [expr, value] of [
    [
      pair([symbol('quote'), symbol('a'), symbol('b')]),
      pair([symbol('a'), symbol('b')]),
    ]
  ]) {
    const interp = new Interpreter();
    expect(interp.evaluate(expr)).toEqual(value);
  }

  for (const expr of [
    pair([symbol('a'), symbol('a'), symbol('c')])
  ]) {
    const interp = new Interpreter();
    expect(interp.evaluate(expr)).toBeInstanceOf(RuntimeError);
  }
});
