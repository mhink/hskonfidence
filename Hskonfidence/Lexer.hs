module Hskonfidence.Lexer
  (
    lexer,
    lexIdent,
    lexIdent',
  ) where

  import Hskonfidence.Token
  import Data.Char
  import Data.Either
  import Data.Maybe

  lexer :: String -> [Token]
  lexer ('w':'h':'i':'l':'s':'t':cs)     = Token WHILST "whilst" : lexer cs
  lexer ('w':'r':'i':'t':'e':cs)         = Token WRITE "write"     : lexer cs
  lexer ('f':'l':'o':'a':'t':cs)         = Token FLOAT "float"     : lexer cs
  lexer ('m':'a':'y':'b':'e':cs)         = Token MAYBE "maybe"     : lexer cs
  lexer ('a':'r':'r':'a':'y':cs)         = Token ARRAY "array"     : lexer cs
  lexer ('c':'h':'a':'r':cs)             = Token CHAR "char"      : lexer cs
  lexer ('i':'s':'i':'s':cs)             = Token TEQ "isis"        : lexer cs
  lexer ('i':'s':'n':'t':cs)             = Token NE "isnt"        : lexer cs
  lexer ('r':'e':'a':'d':cs)             = Token READ "read"      : lexer cs
  lexer ('a':'n':'d':cs)                 = Token AND "and"       : lexer cs
  lexer ('i':'n':'t':cs)                 = Token INT "int"       : lexer cs
  lexer ('n':'o':'t':cs)                 = Token NOT "not"       : lexer cs
  lexer ('o':'r':cs)                     = Token OR "or"        : lexer cs
  lexer ('i':'s':cs)                     = Token ASSIGN "is"    : lexer cs
  lexer ('/':'/':cs)                     = Token FDIV "//"      : lexer cs
  lexer ('<':'=':cs)                     = Token LE "<="        : lexer cs
  lexer ('>':'=':cs)                     = Token GE ">="        : lexer cs
  lexer ('(':cs)                         = Token LPAREN "("    : lexer cs
  lexer (')':cs)                         = Token RPAREN ")"   : lexer cs
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

  lexer (c:cs ) 
    | isSpace c       = lexer             (cs)
    | isAlpha c       = lexIdent          (c:cs)
    | isDigit c       = lexNumer          (c:cs)
    | isDoubleQuote c = lexStringLiteral  (cs)
  lexer _ = [ Token ERROR "error"]

  lexIdent :: String -> [Token]
  lexIdent str = 
    case lexIdent' (str) of
      Nothing -> [Token ERROR "error"]
      Just result ->
        let (resultToken , restOfStr) = result
        in  resultToken  : lexer (restOfStr) 

  lexIdent' :: String -> Maybe (Token, String)
  lexIdent' "" = Just (Token IDENT (""), "")
  lexIdent' (c:cs)
    | isAlphaNum c =             --continue recursion 
        case lexIdent' cs of
          Just (Token IDENT id, rest) -> Just (Token IDENT (c:id), rest)
          Nothing         -> Nothing
    | isSpace c = Just (Token IDENT "", cs)  --end recursion successfully
    | otherwise = Nothing        --end recursion unsuccessfulxy

  lexNumer :: String -> [Token ]
  lexNumer str = 
    let
      (num, sep, rest) = lexNumer' str
    in case sep of
      ' ' -> Token INTLIT num : lexer rest
      '\t' -> Token INTLIT num : lexer rest
      '\n' -> Token INTLIT num : lexer rest
      '.' -> lexFloat (num, rest)
      otherwise  -> [Token ERROR (sep:" ferror")]
      
  lexNumer' :: String -> (String, Char, String)
  lexNumer' [] = ("", ' ', "")
  lexNumer' (c:cs)
    | isDigit c = 
      let (num, sep, rest) = lexNumer' cs
      in  (c:num, sep, rest)
    | otherwise = ("", c, cs)

  lexFloat :: (String, String) -> [Token]
  lexFloat (numToNow, rest) = 
    case lexFloat' rest of
      Nothing -> [Token ERROR "error"]
      Just result ->
        let (fPartResult, restOfString) = result
        in Token FLOLIT (numToNow ++ "." ++ fPartResult) : lexer restOfString

  lexFloat' :: String -> Maybe(String, String)
  lexFloat' "" = Just ("", "")
  lexFloat' (c:cs)
    | isDigit c =
      case lexFloat' cs of
        Just (num, rest) -> Just (c:num, rest)
        Nothing          -> Nothing
    | isSpace c = Just ("", cs)
    | otherwise = Nothing

  lexStringLiteral :: String -> [Token]
  lexStringLiteral str =
    case lexStringLiteral' str of
      Nothing -> [Token ERROR "error"]
      Just (resultToken, rest) -> resultToken : lexer rest

  lexStringLiteral' :: String -> Maybe (Token, String)
  lexStringLiteral' [] = Nothing
  lexStringLiteral' ('"':cs) = Just (Token STRLIT "", cs)
  lexStringLiteral' (c:cs) =
    case lexStringLiteral' cs of
      Just (Token STRLIT (strLit), rest) -> Just (Token STRLIT (c:strLit), rest)
      Nothing -> Nothing

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
