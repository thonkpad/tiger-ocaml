/* KEYWORDS */
%token LET "let"
%token IN "in"
%token END "end"
%token OF "of"
%token BREAK "break"
%token NIL "nil"
%token FUNCTION "function"
%token VAR "var"
%token TYPE "type"
%token IMPORT "import"
%token PRIMITIVE "primitive"

/* CONDITIONAL */
%token IF "if"
%token THEN "then"
%token ELSE "else"

/* LOOPS */
%token WHILE "while"
%token FOR "for"
%token TO "to"
%token DO "do"

/* SYMBOLS */
%token ASSIGN ":="
%token DOT "."
%token COMMA ","
%token COLON ":"
%token SEMICOLON ";"

/* ENCLOSURES */
%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token LCURLY "{"
%token RCURLY "}"

/* ARITHMETIC */
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"

/* NUMERICAL COMPARISON */
%token EQ "="
%token NEQ "<>"
%token LT "<"
%token LE "<="
%token GT ">"
%token GE ">="

/* LOGICAL COMPARISON */
%token AND "&"
%token OR "|"

%token <string> STRING
%token <int> INT
%token <string> ID

%token EOF

/* ASSOCIATIVITY */
%left "+" "-"
%left "*" "/"

%start <unit> main

%%
