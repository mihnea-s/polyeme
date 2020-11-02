#include "parser.h"
#include <complex.h>

typedef struct {
  enum {
    DOT,

    OPEN,
    CLOSE,
    ATSIGN,
    SHARP,
    DOLLAR,
    BACKTICK,

    ATOM_NIL,
    ATOM_INT,
    ATOM_BOOL,
    ATOM_FLOAT,
    ATOM_CHAR,
    ATOM_STR,

    SYMBOL,
    FINAL,
  } kind;

  union {
    long integer;
    double real;
    complex float complx;

    bool boolean;
    char character;
    char *string;
  } as;
} token_t;

static token_t lex(const char **buffer_ptr) {
  const char *buffer = *buffer_ptr;

  while (isspace(*buffer)) {
    buffer++;
  }

  switch (*buffer) {
  case '\0':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = FINAL};
  case '.':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = DOT};
  case '(':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = OPEN};
  case ')':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = CLOSE};
  case '@':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = ATSIGN};
  case '$':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = DOLLAR};
  case '`':
    *buffer_ptr = ++buffer;
    return (token_t){.kind = BACKTICK};
  }

  if (*buffer == '#') {
    switch (*(++buffer)) {
    case 't':
      *buffer_ptr = ++buffer;
      return (token_t){.kind = ATOM_BOOL, .as.boolean = true};
    case 'f':
      *buffer_ptr = ++buffer;
      return (token_t){.kind = ATOM_BOOL, .as.boolean = false};
    default:
      *buffer_ptr = buffer;
      return (token_t){.kind = SHARP};
    }
  }

  if (*buffer == '\'') {
    const char chr = *(++buffer);

    // Skip the character itself
    if (*buffer != '\0') {
      buffer++;
    }

    *buffer_ptr = buffer;
    return (token_t){
        .kind = ATOM_CHAR,
        .as.character = chr,
    };
  }

  if (*buffer == '"') {
    const char *begin = ++buffer;

    while ((*buffer != '"') && (*buffer != '\0')) {
      buffer++;
    }

    const char *end = buffer;

    if (*buffer == '"') {
      buffer++;
    }

    char *text = malloc(end - begin + 1);
    strncpy(text, begin, end - begin);
    text[end - begin] = '\0';

    *buffer_ptr = buffer;
    return (token_t){
        .kind = ATOM_STR,
        .as.string = text,
    };
  }

  if (isdigit(*buffer)) {
    const char *begin = buffer;

    while (isdigit(*buffer) && (*buffer != '\0')) {
      buffer++;
    }

    if (*buffer != ',') {
      *buffer_ptr = buffer;
      return (token_t){
          .kind = ATOM_INT,
          .as.integer = strtol(begin, NULL, 0),
      };
    }

    const size_t comma = (buffer++) - begin;

    // Skip decimal part
    while (isdigit(*buffer) && (*buffer != '\0')) {
      buffer++;
    }

    char *number = malloc(buffer - begin + 1);
    strncpy(number, begin, buffer - begin);

    number[comma] = '.';
    number[buffer - begin] = '\0';

    const double floating = strtod(number, NULL);
    free(number);

    *buffer_ptr = buffer;
    return (token_t){
        .kind = ATOM_FLOAT,
        .as.real = floating,
    };
  }

  const char *begin = buffer;

  while (!isspace(*buffer) && !(*buffer == '\0')) {
    buffer++;
  }

  char *word = malloc(buffer - begin + 1);
  strncpy(word, begin, buffer - begin);
  word[buffer - begin] = '\0';

  *buffer_ptr = buffer;
  return (token_t){
      .kind = SYMBOL,
      .as.string = word,
  };
}

static void discard(token_t token) {
  if (token.kind == SYMBOL || token.kind == ATOM_STR) {
    free(token.as.string);
  }
}

/* TODO: Documentation */
po_parser_t po_parser_init(const char *buffer, const char *source) {
  return (po_parser_t){
      .buffer = buffer,
      .source = source,
      .errors = po_array_init(0, po_parser_error_t),
  };
}

void po_parser_parse(po_parser_t parser) {}

/* TODO: Documentation */
void po_parser_deinit(po_parser_t parser) {
  po_parser_error_t error;
  po_array_iterate(parser.errors, error) { free((void *)error.error); };
  po_array_deinit(parser.errors);
}