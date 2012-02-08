module Hskonfidence.Lexer
  (
    lexer,
    lexIdent,
    lexIdent'
  ) where

  import Hskonfidence.Token
  import Data.Char
  import Data.Either
  import Data.Maybe

  lexer :: String -> [Token]
  lexer ('w':'h':'i':'l':'s':'t':cs)     = TokenWHILST    : lexer cs
  lexer ('w':'r':'i':'t':'e':cs)         = TokenWRITE     : lexer cs
  lexer ('f':'l':'o':'a':'t':cs)         = TokenFLOAT     : lexer cs
  lexer ('m':'a':'y':'b':'e':cs)         = TokenMAYBE     : lexer cs
  lexer ('a':'r':'r':'a':'y':cs)         = TokenARRAY     : lexer cs
  lexer ('c':'h':'a':'r':cs)             = TokenCHAR      : lexer cs
  lexer ('i':'s':'i':'s':cs)             = TokenEQ        : lexer cs
  lexer ('i':'s':'n':'t':cs)             = TokenNE        : lexer cs
  lexer ('r':'e':'a':'d':cs)             = TokenREAD      : lexer cs
  lexer ('a':'n':'d':cs)                 = TokenAND       : lexer cs
  lexer ('i':'n':'t':cs)                 = TokenINT       : lexer cs
  lexer ('n':'o':'t':cs)                 = TokenNOT       : lexer cs
  lexer ('o':'r':cs)                     = TokenOR        : lexer cs
  lexer ('i':'s':cs)                     = TokenASSIGN    : lexer cs
  lexer ('/':'/':cs)                     = TokenFDIV      : lexer cs
  lexer ('<':'=':cs)                     = TokenLE        : lexer cs
  lexer ('>':'=':cs)                     = TokenGE        : lexer cs
  lexer ('(':cs)                         = TokenLPAREN    : lexer cs
  lexer (')':cs)                         = TokenRPAREN    : lexer cs
  lexer ('[':cs)                         = TokenLBRACK    : lexer cs
  lexer (']':cs)                         = TokenRBRACK    : lexer cs
  lexer ('{':cs)                         = TokenLBRACE    : lexer cs
  lexer (',':cs)                         = TokenCOMMA     : lexer cs
  lexer ('_':cs)                         = TokenNEG       : lexer cs
  lexer ('+':cs)                         = TokenPLUS      : lexer cs
  lexer ('-':cs)                         = TokenMINUS     : lexer cs
  lexer ('*':cs)                         = TokenTIMES     : lexer cs
  lexer ('/':cs)                         = TokenDIV       : lexer cs
  lexer ('%':cs)                         = TokenMOD       : lexer cs
  lexer ('<':cs)                         = TokenLT        : lexer cs
  lexer ('>':cs)                         = TokenGT        : lexer cs
  lexer ('?':cs)                         = TokenTERM      : lexer cs
  lexer [] = []

  lexer (c:cs ) 
    | isSpace c = lexer    (cs)
    | isAlpha c = lexIdent (c:cs)
    | isDigit c = lexNumer (c:cs)
  lexer _ = [ TokenERROR ]

  lexIdent :: String -> [Token]
  lexIdent str = 
      case lexIdent' (str) of
        Nothing -> [TokenERROR]
        Just result ->
          let (resultStr, restOfStr) = result
          in  TokenIDENT (resultStr) : lexer (restOfStr) 

  lexIdent' :: String -> Maybe (String, String)
  lexIdent' "" = Just ("", "")
  lexIdent' (c:cs)
    | isAlphaNum c =             --continue recursion 
        case lexIdent' cs of
          Just (id, rest) -> Just (c:id, rest)
          Nothing         -> Nothing
    | isSpace c = Just ([], cs)  --end recursion successfully
    | otherwise = Nothing        --end recursion unsuccessfully

  lexNumer :: String -> [Token]
  lexNumer (c:cs) = TokenERROR : lexer cs


  ----Convenience methods
  isDoubleQuote :: Char -> Bool 
  isDoubleQuote '"' = True
  isDoubleQuote x = False

  isSingleQuote :: Char -> Bool
  isSingleQuote '\'' = True
  isSingleQuote x = False

  isDecimalPoint :: Char -> Bool
  isDecimalPoint '.' = True
  isDecimalPoint x = False
