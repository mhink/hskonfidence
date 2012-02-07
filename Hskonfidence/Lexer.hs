module Hskonfidence.Lexer
  (
    lexer
  ) where

  import Hskonfidence.Token

  lexer :: String -> [Token]
  lexer [] = [ EOF ]

  lexer c:cs 
    | isSpace c = lexer cs
    | isAlpha c = lexVar c:cs
    | isDigit c = lexNum c:cs

  lexer ("(":cs)      = TokenLPAREN   : lexer cs
  lexer (")":cs)      = TokenRPAREN   : lexer cs
  lexer ("[":cs)      = TokenLBRACK   : lexer cs
  lexer ("]":cs)      = TokenRBRACK   : lexer cs
  lexer ("{":cs)      = TokenLBRACE   : lexer cs
  lexer (",":cs)      = TokenCOMMA    : lexer cs
  lexer ("_":cs)      = TokenNEG      : lexer cs
  lexer ("+":cs)      = TokenPLUS     : lexer cs
  lexer ("-":cs)      = TokenMINUS    : lexer cs
  lexer ("*":cs)      = TokenTIMES    : lexer cs
  lexer ("/":cs)      = TokenDIV      : lexer cs
  lexer ("//":cs)     = TokenFDIV     : lexer cs
  lexer ("%":cs)      = TokenMOD      : lexer cs
  lexer ("is":cs)     = TokenASSIGN   : lexer cs
  lexer ("isis":cs)   = TokenEQ       : lexer cs
  lexer ("isnt":cs)   = TokenNE       : lexer cs
  lexer ("<=":cs)     = TokenLE       : lexer cs
  lexer ("<":cs)      = TokenLT       : lexer cs
  lexer (">=":cs)     = TokenGE       : lexer cs
  lexer (">":cs)      = TokenGT       : lexer cs
  lexer ("and":cs)    = TokenAND      : lexer cs
  lexer ("or":cs)     = TokenOR       : lexer cs
  lexer ("not":cs)    = TokenNOT      : lexer cs
  lexer ("?":cs)      = TokenTERM : lexer cs
  lexer ("array":cs)  = TokenARRAY : lexer cs
  lexer ("char":cs)   = TokenCHAR : lexer cs
  lexer ("float":cs)  = TokenFLOAT : lexer cs
  lexer ("maybe":cs)  = TokenMAYBE : lexer cs
  lexer ("int":cs)    = TokenINT : lexer cs
  lexer ("read":cs) = TokenREAD : lexer cs
  lexer ("whilst":cs) = TokenWHILST : lexer cs
  lexer ("write":cs) = TokenWRITE : lexer cs

  --Convenience methods
  isDoubleQuote :: Char -> Bool 
  isDoubleQuote '"' = True
  isDoubleQuote x = False

  isSingleQuote :: Char -> Bool
  isSingleQuote '\'' = True
  isSingleQuote x = False

  isDecimalPoint :: Char -> Bool
  isDecimalPoint '.' = True
  isDecimalPoint x = False
