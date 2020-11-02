#ifndef POLYEME_INTERP_H
#define POLYEME_INTERP_H

#include "array.h"

struct po_sexp_t;

typedef struct po_sexp_t po_sexp_t;
typedef po_array_t(po_sexp_t) po_array_sexp_t;

struct po_sexp_t {
  po_array_sexp_t children;
};

#endif