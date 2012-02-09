module Hskonfidence.Lexer
  (
    lexer,
    lexIdent,
    lexIdent',
    lexNumer,
    lexNumer'
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

  lexer ('\'':c:'\'':cs)                 = TokenCHRLIT c : lexer cs

  lexer (c:cs ) 
    | isSpace c       = lexer             (cs)
    | isAlpha c       = lexIdent          (c:cs)
    | isDigit c       = lexNumer          (c:cs)
    | isDoubleQuote c = lexStringLiteral  (cs)
  lexer _ = [ TokenERROR ]

  lexIdent :: String -> [Token]
  lexIdent str = 
    case lexIdent' (str) of
      Nothing -> [TokenERROR]
      Just result ->
        let (resultToken, restOfStr) = result
        in  resultToken : lexer (restOfStr) 

  lexIdent' :: String -> Maybe (Token, String)
  lexIdent' "" = Just (TokenIDENT (""), "")
  lexIdent' (c:cs)
    | isAlphaNum c =             --continue recursion 
        case lexIdent' cs of
          Just (TokenIDENT (id), rest) -> Just (TokenIDENT(c:id), rest)
          Nothing         -> Nothing
    | isSpace c = Just (TokenIDENT "", cs)  --end recursion successfully
    | otherwise = Nothing        --end recursion unsuccessfully

  lexNumer :: String -> [Token]
  lexNumer str = 
    let
      (num, sep, rest) = lexNumer' str
    in case sep of
      ' ' -> TokenINTLIT (read num :: Integer) : lexer rest
      '.' -> lexFloat (num, rest)
      otherwise  -> [TokenERROR]
      
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
      Nothing -> [TokenERROR]
      Just result ->
        let (fPartResult, restOfString) = result
        in TokenFLOLIT (read (numToNow ++ "." ++ fPartResult) :: Float) : lexer restOfString

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
      Nothing -> [TokenERROR]
      Just (resultToken, rest) -> resultToken : lexer rest

  lexStringLiteral' :: String -> Maybe (Token, String)
  lexStringLiteral' [] = Nothing
  lexStringLiteral' ('"':cs) = Just (TokenSTRLIT "", cs)
  lexStringLiteral' (c:cs) =
    case lexStringLiteral' cs of
      Just (TokenSTRLIT (strLit), rest) -> Just (TokenSTRLIT(c:strLit), rest)
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
