module Hskonfidence.Token
  (
    Token(..) 
  ) where

  data Token = 
    LPAREN      | --T1  Left Parenthesis      (
    RPAREN      | --T2  Right Parenthesis     )
    LBRACK      | --T3  Left Bracket          [
    RBRACK      | --T4  Right Bracket         ]
    LBRACE      | --T5  Left Brace            {
    RBRACE      | --T6  Right Brace           }
    COMMA       | --T7  Comma                 ,
    NEG         | --T8  Unary negation        _ (underscore)
    PLUS        | --T9  Addition operator     +
    MINUS       | --T10 Subtraction operator  -
    TIMES       | --T11 Multiply operator     *
    DIV         | --T12 Integer division op.  /
    FDIV        | --T13 Float division op.    //
    MOD         | --T14 Modulo operator       %
    ASSIGN      | --T15 Assignment operator   is
    EQ          | --T16 Equality operator     isis
    NE          | --T17 Inequality operator   isnt
    LE          | --T18 Less-than-equals op.  <=
    LT          | --T19 Less-than op.         <
    GE          | --T20 Greater-than-equals   >=
    GT          | --T21 Greater-than op.      >
    AND         | --T22 Boolean AND op.       and
    OR          | --T23 Boolean OR op.        or
    NOT         | --T24 Boolean NOT op.       not
    TERM        | --T25 Line terminator       ?
    ARRAY       | --T26 Array keyword         array
    CHAR        | --T27 Character primitive   char
    FLOAT       | --T28 Float primitive ind.  float
    MAYBE       | --T29 if-statement          maybe
    INT         | --T30 Integer primitive     int
    READ        | --T31 Read from stream      read
    WHILST      | --T32 while-loop            whilst
    WRITE       | --T33 Write to stream       write
    TRUE        | --T34 Boolean true literal  true
    FALSE       | --T35 Boolean false literal false
    IDENT   String    | --T36 Identifier            [a-zA-Z][0-9a-zA-Z]*
    INTLIT  String    | --T37 Integer literal       [0-9]+
    FLOLIT  String    | --T38 Float literal         [0-9]+\.[0-9]*
    CHRLIT  String    | --T39 Char literal          '[a-zA-Z]'
    STRLIT  String    | --T40 String literal        "[a-zA-Z]*"
    EOF
      deriving (
        Show
      )

