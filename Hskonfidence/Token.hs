module Hskonfidence.Token
  (
    Token(..)     ,
  ) where

  data Token = 
    TokenLPAREN            | --T1  Left Parenthesis      (
    TokenRPAREN            | --T2  Right Parenthesis     )
    TokenLBRACK            | --T3  Left Bracket          [
    TokenRBRACK            | --T4  Right Bracket         ]
    TokenLBRACE            | --T5  Left Brace            {
    TokenRBRACE            | --T6  Right Brace           }
    TokenCOMMA             | --T7  Comma                 ,
    TokenNEG               | --T8  Unary negation        _ (underscore)
    TokenPLUS              | --T9  Addition operator     +
    TokenMINUS             | --T10 Subtraction operator  -
    TokenTIMES             | --T11 Multiply operator     *
    TokenDIV               | --T12 Integer division op.  /
    TokenFDIV              | --T13 Float division op.    //
    TokenMOD               | --T14 Modulo operator       %
    TokenASSIGN            | --T15 Assignment operator   is
    TokenEQ               | --T16 Equality operator     isis
    TokenNE               | --T17 Inequality operator   isnt
    TokenLE               | --T18 Less-than-equals op.  <=
    TokenLT               | --T19 Less-than op.         <
    TokenGE               | --T20 Greater-than-equals   >=
    TokenGT               | --T21 Greater-than op.      >
    TokenAND               | --T22 Boolean AND op.       and
    TokenOR                | --T23 Boolean OR op.        or
    TokenNOT               | --T24 Boolean NOT op.       not
    TokenTERM              | --T25 Line terminator       ?
    TokenARRAY             | --T26 Array keyword         array
    TokenCHAR              | --T27 Character primitive   char
    TokenFLOAT             | --T28 Float primitive ind.  float
    TokenMAYBE             | --T29 if-statement          maybe
    TokenINT               | --T30 Integer primitive     int
    TokenREAD              | --T31 Read from stream      read
    TokenWHILST            | --T32 while-loop            whilst
    TokenWRITE             | --T33 Write to stream       write
    TokenIDENT   String    | --T34 Identifier            [a-zA-Z][0-9a-zA-Z]*
    TokenINTLIT  Integer   | --T35 Integer literal       [0-9]+
    TokenFLOLIT  Float     | --T36 Float literal         [0-9]+\.[0-9]*
    TokenCHRLIT  Char      | --T37 Char literal          '[a-zA-Z]'
    TokenSTRLIT  String    | --T38 String literal        "[a-zA-Z]*"
    TokenEOF |
    TokenERROR
      deriving (
        Show
      )
