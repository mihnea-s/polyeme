#ifndef POLYEME_AST_H
#define POLYEME_AST_H

#include <complex.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct {
  enum {
    // Literals
    PO_VAL_BOLN,
    PO_VAL_CHRT,
    PO_VAL_INTG,
    PO_VAL_REAL,
    PO_VAL_CMPX,
    PO_VAL_VECT,
    PO_VAL_LIST,
  } kind;

  union {
    bool boln;
    char chrt;
    int32_t intg;
    double real;
    complex float cmpx;

  } as;
} po_value_t;

#endif