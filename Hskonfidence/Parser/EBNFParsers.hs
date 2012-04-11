module Hskonfidence.Parser.EBNFParsers 
  where

  import Hskonfidence.Parser.RealParser
  import Hskonfidence.Token
  
  item :: Parser Token
  item = Parser (\ts -> case ts of
                          [] -> []
                          (t:ts) -> [(t, ts)])

  token :: TokenType -> Parser Token
  token tt =  do Token tt' str <- item 
                 case tt == tt' of
                    True -> return (Token tt' str)
                    False -> mzero

  many :: Parser a -> Parser [a]
  many p = many1 p <|> return []

  many1 :: Parser a -> Parser [a]
  many1 p = do a <- p
               as <- many p
               return (a:as)

  zro :: Parser a -> Parser (Maybe a)
  zro p = do {  result <- p ;
                  return (Just result) } <|>
            return Nothing
  
  separated_with :: Parser a -> Parser b -> Parser (a, [(b, a)])
  separated_with a b =
    do a'   <- a
       bas' <- many (sepwith a b)
       return (a', bas')
  
  sepwith :: Parser a -> Parser b -> Parser (b, a)
  sepwith a b =
    do b' <- b
       a' <- a
       return (b', a')
  
  separated_by :: Parser a -> Parser b -> Parser [a]
  separated_by a b =
    do a'  <- a
       as' <- many (sepby' a b)
       return (a':as')
  
  sepby' :: Parser a -> Parser b -> Parser a
  sepby' a b =
    do b
       a' <- a
       return (a')
       
  string :: TokenType -> Parser String
  string tt =
    do (Token _ str) <- (token tt)
       return str