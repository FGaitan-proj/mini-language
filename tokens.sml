structure Tokens =
struct

  datatype token =
    ID of Ast.ident
  | INT of int
  | REAL of real
  | CHAR of char
  | STRING of string
  | TRUE
  | FALSE
  | ORELSE 
  | ANDALSO 
  | PLUS 
  | MINUS 
  | TIMES 
  | DIV
  | PLUSPOUND
  | MINUSPOUND
  | TIMESPOUND
  | DIVPOUND
  | MOD
  | EQ 
  | NE
  | LT
  | LE
  | GT
  | GE
  | LTPOUND
  | LEPOUND
  | GTPOUND
  | GEPOUND
  | CAT
  | LP
  | RP
  | LBRACK
  | RBRACK
  | NIL
  | DCOLON
  | AT
  | FN
  | DARROW
  | LET
  | IN
  | END
  | IF
  | THEN
  | ELSE
  | SEL of int
  | COMMA
  | VAL
  | FUN
  | SEMI
  | WILD
  | EOF

  fun ==(t0 : token, t1 : token) : bool =
    case (t0, t1) of
          (ID(s), ID(s')) => s = s'
        | (INT(n), INT(n')) => n = n'
        | (REAL(n), REAL(n')) => Real.==(n, n')
        | (CHAR c, CHAR c') => c = c'
        | (STRING(s), STRING(s')) => s = s'
        | (SEL(n), SEL(n')) => n = n'
        |  (
             (TRUE, TRUE)
           | (FALSE, FALSE)
           | (ORELSE, ORELSE)
           | (ANDALSO, ANDALSO)
           | (PLUS, PLUS)
           | (MINUS, MINUS)
           | (TIMES, TIMES)
           | (DIV, DIV)
           | (MOD, MOD)
           | (PLUSPOUND, PLUSPOUND)
           | (MINUSPOUND, MINUSPOUND)
           | (TIMESPOUND, TIMESPOUND)
           | (DIVPOUND, DIVPOUND)
           | (EQ, EQ)
           | (NE, NE)
           | (LT, LT)
           | (LE, LE)
           | (GT, GT)
           | (GE, GE)
           | (LTPOUND, LTPOUND)
           | (LEPOUND, LEPOUND)
           | (GTPOUND, GTPOUND)
           | (GEPOUND, GEPOUND)
           | (CAT, CAT)
           | (LP, LP)
           | (RP, RP)
           | (LBRACK, LBRACK)
           | (RBRACK, RBRACK)
           | (NIL, NIL)
           | (DCOLON, DCOLON)
           | (AT, AT)
           | (FN, FN)
           | (DARROW, DARROW)
           | (LET, LET)
           | (IN, IN)
           | (END, END)
           | (IF, IF)
           | (THEN, THEN)
           | (ELSE, ELSE)
           | (COMMA, COMMA)
           | (VAL, VAL)
           | (FUN, FUN)
           | (SEMI, SEMI)
           | (WILD, WILD)
           | (EOF, EOF)
           ) => true
        | _ => false

  fun toString(t : token) : string =
    case t of
           ID(s)               => String.concat ["ID(", s, ")"]
         | INT(n)              => String.concat ["INT(", Int.toString n, ")"]
         | REAL(n)             => String.concat ["REAL(", Real.toString n, ")"]
         | CHAR c              => String.concat ["CHAR(", Char.toString c, ")"]
         | STRING(s)           => String.concat ["STRING(", s, ")"]
         | TRUE                => "TRUE"
         | FALSE               => "FALSE"
         | ORELSE              => "ORELSE"
         | ANDALSO             => "ANDALSO"
         | PLUS                => "PLUS"
         | MINUS               => "MINUS"
         | TIMES               => "TIMES"
         | DIV                 => "DIV"
         | MOD                 => "MOD"
         | PLUSPOUND           => "PLUSPOUND"
         | MINUSPOUND          => "MINUSPOUND"
         | TIMESPOUND          => "TIMESPOUND"
         | DIVPOUND            => "DIVPOUND"
         | EQ                  => "EQ"
         | NE                  => "NE"
         | LT                  => "LT"
         | LE                  => "LE"
         | GT                  => "GT"
         | GE                  => "GE"
         | LTPOUND             => "LTPOUND"
         | LEPOUND             => "LEPOUND"
         | GTPOUND             => "GTPOUND"
         | GEPOUND             => "GEPOUND"
         | CAT                 => "CAT"
         | LP                  => "LPAREN"
         | RP                  => "RPAREN"
         | LBRACK              => "LBRACK"
         | RBRACK              => "RBRACK"
         | NIL                 => "NIL"
         | DCOLON              => "DCOLON"
         | AT                  => "AT"
         | FN                  => "FN"
         | DARROW              => "DARROW"
         | LET                 => "LET"
         | IN                  => "IN"
         | END                 => "END"
         | IF                  => "IF"
         | THEN                => "THEN"
         | ELSE                => "ELSE"
         | SEL(n)              => String.concat ["SEL", Int.toString n]
         | COMMA               => "COMMA"
         | VAL                 => "VAL"
         | FUN                 => "FUN"
         | SEMI                => "SEMI"
         | WILD                => "WILD"
         | EOF                 => "EOF"

end
