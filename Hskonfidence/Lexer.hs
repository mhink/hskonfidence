module Hskonfidence.Lexer
  (
    lexer
  ) where

  import Hskonfidence.Token
  import Data.Char

  lexer :: String -> [Token]
  lexer [] = [ TokenEOF ]
  lexer ('w':'h':'i':'l':'s':'t':cs) = TokenWHILST : lexer cs
  lexer ('w':'r':'i':'t':'e':cs) = TokenWRITE : lexer cs
  lexer ('f':'l':'o':'a':'t':cs)  = TokenFLOAT : lexer cs
  lexer ('m':'a':'y':'b':'e':cs)  = TokenMAYBE : lexer cs
  lexer ('a':'r':'r':'a':'y':cs)  = TokenARRAY : lexer cs
  lexer ('c':'h':'a':'r':cs)   = TokenCHAR : lexer cs
  lexer ('i':'s':'i':'s':cs)   = TokenEQ       : lexer cs
  lexer ('i':'s':'n':'t':cs)   = TokenNE       : lexer cs
  lexer ('r':'e':'a':'d':cs) = TokenREAD : lexer cs
  lexer ('a':'n':'d':cs)    = TokenAND      : lexer cs
  lexer ('i':'n':'t':cs)    = TokenINT : lexer cs
  lexer ('n':'o':'t':cs)    = TokenNOT      : lexer cs
  lexer ('o':'r':cs) =    TokenOR       : lexer cs
  lexer ('i':'s':cs)     = TokenASSIGN   : lexer cs
  lexer ('/':'/':cs)     = TokenFDIV     : lexer cs
  lexer ('<':'=':cs)     = TokenLE       : lexer cs
  lexer ('>':'=':cs)     = TokenGE       : lexer cs
  lexer ('(':cs)      = TokenLPAREN   : lexer cs
  lexer (')':cs)      = TokenRPAREN   : lexer cs
  lexer ('[':cs)      = TokenLBRACK   : lexer cs
  lexer (']':cs)      = TokenRBRACK   : lexer cs
  lexer ('{':cs)      = TokenLBRACE   : lexer cs
  lexer (',':cs)      = TokenCOMMA    : lexer cs
  lexer ('_':cs)      = TokenNEG      : lexer cs
  lexer ('+':cs)      = TokenPLUS     : lexer cs
  lexer ('-':cs)      = TokenMINUS    : lexer cs
  lexer ('*':cs)      = TokenTIMES    : lexer cs
  lexer ('/':cs)      = TokenDIV      : lexer cs
  lexer ('%':cs)      = TokenMOD      : lexer cs
  lexer ('<':cs)      = TokenLT       : lexer cs
  lexer ('>':cs)      = TokenGT       : lexer cs
  lexer ('?':cs)      = TokenTERM : lexer cs
  lexer ( c:cs ) 
    | isSpace c = lexer cs
    | isAlpha c = lexer cs
    | isDigit c = lexer cs
  lexer _ = [ TokenERROR ]

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
