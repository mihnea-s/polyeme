#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <linenoise/linenoise.h>

#include "array.h"
#include "color.h"
#include "parser.h"

static const char prompt_idle[] = BOLD FG_GRAY "po" RESET BOLD " >> " RESET;
static const char prompt_err[] = BOLD FG_RED "po" RESET BOLD " >> " RESET;
static const char prompt_ok[] = BOLD FG_GREEN "po" RESET BOLD " >> " RESET;

int main() {
  // Version information
  printf("Polymer version " FG_GREEN POLYMER_VERSION FG_BLUE BOLD " C" RESET
         ".\n");
  printf("Using Polyeme " FG_GREEN POLYEME_VERSION FG_BLUE BOLD " C" RESET
         ".\n");
  printf("Press " BOLD "Ctrl-C" RESET " to quit.\n\n");

  char *input = NULL;
  const char *prompt = prompt_idle;

  while ((input = linenoise(prompt)) != NULL) {
    linenoiseHistoryAdd(input);

    po_parser_t parser = po_parser_init(input, "<stdin>");
    po_parser_parse(parser);
    po_parser_deinit(parser);

    prompt = prompt_ok;

    free(input);
  }

  return EXIT_SUCCESS;
}