{
    open Parser
}

let digit = ['0'-'9']
let identif_start = ['a'-'z' 'A'-'Z' "!#$%&|*+-/:<=>?@^_~"]
let identif_var   = digit [identif_start digit]+

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ".."            { DOTDOT }
  | '.'             { DOT }
  | '\''            { QUOTE }
  | '`'             { BACKTICK }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "#f"            { HASHFALSE }
  | "#t"            { HASHTRUE }
  | identif_var     { VAR (Lexing.lexeme lexbuf) }
  | digit+          { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | eof             { EOF }
