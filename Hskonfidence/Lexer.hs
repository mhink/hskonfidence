module Hskonfidence.Lexer
  (
    lexer,
    getTokenStringFrom,
    lexIdent,
    lexStrLit
  ) where

  import Hskonfidence.Token
  import Data.Char
  import Data.Either
  import Data.Maybe

  lexer :: String -> [Token]
  lexer ('w':'h':'i':'l':'s':'t':cs)     = Token WHILST "whilst" : lexer cs
  lexer ('w':'r':'i':'t':'e':cs)         = Token WRITE "write"      : lexer cs
  lexer ('f':'l':'o':'a':'t':cs)         = Token FLOAT "float"      : lexer cs
  lexer ('m':'a':'y':'b':'e':cs)         = Token MAYBE "maybe"      : lexer cs
  lexer ('a':'r':'r':'a':'y':cs)         = Token ARRAY "array"      : lexer cs
  lexer ('c':'h':'a':'r':cs)             = Token CHAR "char"        : lexer cs
  lexer ('i':'s':'i':'s':cs)             = Token TEQ "isis"         : lexer cs
  lexer ('i':'s':'n':'t':cs)             = Token NE "isnt"          : lexer cs
  lexer ('r':'e':'a':'d':cs)             = Token READ "read"        : lexer cs
  lexer ('a':'n':'d':cs)                 = Token AND "and"          : lexer cs
  lexer ('i':'n':'t':cs)                 = Token INT "int"          : lexer cs
  lexer ('n':'o':'t':cs)                 = Token NOT "not"          : lexer cs
  lexer ('o':'r':cs)                     = Token OR "or"            : lexer cs
  lexer ('i':'s':cs)                     = Token ASSIGN "is"        : lexer cs
  lexer ('/':'/':cs)                     = Token FDIV "//"          : lexer cs
  lexer ('<':'=':cs)                     = Token LE "<="            : lexer cs
  lexer ('>':'=':cs)                     = Token GE ">="            : lexer cs
  lexer ('(':cs)                         = Token LPAREN "("         : lexer cs
  lexer (')':cs)                         = Token RPAREN ")"         : lexer cs
  lexer ('[':cs)                         = Token LBRACK "["    : lexer cs
  lexer (']':cs)                         = Token RBRACK "]"    : lexer cs
  lexer ('{':cs)                         = Token LBRACE "{"    : lexer cs
  lexer ('}':cs)                         = Token RBRACE "}"    : lexer cs
  lexer (',':cs)                         = Token COMMA ","     : lexer cs
  lexer ('_':cs)                         = Token NEG "_"       : lexer cs
  lexer ('+':cs)                         = Token PLUS "+"      : lexer cs
  lexer ('-':cs)                         = Token MINUS "-"     : lexer cs
  lexer ('*':cs)                         = Token TIMES "*"     : lexer cs
  lexer ('/':cs)                         = Token DIV "/"       : lexer cs
  lexer ('%':cs)                         = Token MOD "%"       : lexer cs
  lexer ('<':cs)                         = Token TLT "<"        : lexer cs
  lexer ('>':cs)                         = Token TGT ">"        : lexer cs
  lexer ('?':cs)                         = Token TERM "?"     : lexer cs
  lexer [] = []
  lexer ('\'':c:'\'':cs)                 = Token CHRLIT [c] : lexer cs

  lexer str 
    | isSpace $ head str = lexer $ tail str
    | isAlpha $ head str = lexIdent  tokenString    : lexer trest
    | isDigit $ head str = lexNumer  tokenString    : lexer trest
    | isQuote $ head str = Token STRLIT quoteString : lexer qrest
    | isSingleQuote $ head str = Token ERROR tokenString : lexer trest
      where (tokenString, trest) = getTokenStringFrom   str
            (quoteString, qrest) = getQuoteStringFrom $ tail str
      
  getTokenStringFrom :: String -> (String, String)
  getTokenStringFrom [] = ("", "")
  getTokenStringFrom (c:cs)
    | isSpace c = ("", cs)
    | otherwise =
        let (result, rest) = getTokenStringFrom cs
        in  (c:result, rest)

  getQuoteStringFrom :: String -> (String, String)
  getQuoteStringFrom [] = ("", "")
  getQuoteStringFrom(c:cs)
    | isQuote c = ("", cs)
    | otherwise =
        let (result, rest) = getQuoteStringFrom cs
        in  (c:result, rest)

  lexIdent :: String -> Token
  lexIdent [] = Token IDENT ""
  lexIdent (c:cs) 
    | isAlphaNum c =
        let Token tokenType ident = lexIdent cs
        in  Token tokenType (c:ident)
    | otherwise = Token ERROR (c:cs)

  lexStrLit :: String -> Token
  lexStrLit [] = Token STRLIT ""
  lexStrLit (c:cs)
    | isQuote c = Token STRLIT ""
    | otherwise = let Token tokenType strLit = lexStrLit cs
                  in  Token tokenType (c:strLit)

  lexNumer :: String -> Token
  lexNumer []       = Token INTLIT ""
  lexNumer (c:cs)
    | isDigit c =
        let Token tokenType numer = lexNumer cs
        in  Token tokenType (c:numer)
    | isDecimal c =
        let Token tokenType numer = lexFloat cs
        in  Token tokenType (c:numer)
    | otherwise = Token ERROR (c:cs)

  lexFloat :: String -> Token
  lexFloat []      = Token FLOLIT ""
  lexFloat (c:cs) 
    | isDigit c =
        let Token tokenType float = lexFloat cs
        in  Token tokenType (c:float)
    | otherwise = Token ERROR (c:cs)

  isQuote :: Char -> Bool
  isQuote '"' = True
  isQuote  _  = False

  isSingleQuote :: Char -> Bool
  isSingleQuote '\'' = True
  isSingleQuote  _   = False

  isDecimal :: Char -> Bool
  isDecimal '.' = True
  isDecimal  _  = False
