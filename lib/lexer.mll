{
open Lexing
open Parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let int = digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = ['a'-'z'] alphanumeric*

rule read = parse
     | white { read lexbuf }
     | newline { new_line lexbuf; read lexbuf }
     | '"' {read_string (Buffer.create 16) lexbuf}
     | "/*" { read_comment [lexbuf.lex_curr_p] lexbuf }

     | "type" { TYPE }
     | "var" { VAR }
     | "function" { FUNCTION }
     | "break" { BREAK }
     | "of" { OF }
     | "end" { END }
     | "in" { IN }
     | "nil" { NIL }
     | "let" { LET }
     | "array" { ARRAY }

     | "do" { DO }
     | "to" { TO }
     | "for" { FOR }
     | "while" { WHILE }

     | "else" { ELSE }
     | "then" { THEN }
     | "if" { IF }

     | ":=" { ASSIGN }

     | "|" { OR }
     | "&" { AND }

     | ">=" { GE }
     | ">" { GT }
     | "<=" { LE }
     | "<" { LT }
     | "<>" { NEQ }
     | "=" { EQ }

     | "+" { PLUS }
     | "-" { MINUS }
     | "*" { TIMES }
     | "/" { DIV }

     | "." { DOT }
     | "{" { LBRACE }
     | "}" { RBRACE }
     | "[" { LBRACK }
     | "]" { RBRACK }
     | "(" { LPAREN }
     | ")" { RPAREN }
     | ";" { SEMICOLON }
     | ":" { COLON }
     | "," { COMMA }

     | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
     | id { ID (Lexing.lexeme lexbuf) }
     | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
     | eof {EOF}

and read_string buf = parse
    | '"' { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
      }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError "String is not terminated") }

and read_comment opened = parse
    | "/*" { read_comment (lexbuf.lex_curr_p::opened) lexbuf }
    | "*/"
      { match opened with
        | _::[] -> read lexbuf
        | _ -> read_comment (List.tl opened) lexbuf
      }
    | newline { new_line lexbuf; read_comment opened lexbuf }
    | _ { read_comment opened lexbuf }
    | eof
      { lexbuf.lex_curr_p <- List.hd opened;
        raise (SyntaxError "Unterminated comment")
      }
