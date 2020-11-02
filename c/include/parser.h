#ifndef POLYEME_PARSER_H
#define POLYEME_PARSER_H

#include <complex.h>
#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"

/* TODO: Documentation */
typedef struct {
  size_t line, column;
  const char *error;
} po_parser_error_t;

typedef po_array_t(po_parser_error_t) po_array_parser_error_t;

/* TODO: Documentation */
typedef struct {
  const char *buffer;
  const char *source;
  po_array_parser_error_t errors;
} po_parser_t;

/* TODO: Documentation */
po_parser_t po_parser_init(const char *buffer, const char *source);

/* TODO: Documentation */
void po_parser_parse(po_parser_t parser);

/* TODO: Documentation */
void po_parser_deinit(po_parser_t parser);

#endif