import { Datum, DatumKind } from "./datum";

export function isTruthy(datum: Datum) {
  return !(datum.kind == DatumKind.Boolean && datum.value == false);
}


