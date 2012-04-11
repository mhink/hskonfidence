module Hskonfidence.Parser.RealParser (
    Parser(..),
    eval,
    return,
    (>>=),
    mzero,
    mplus,
    (<|>)
  )
  where
  
  import Control.Monad
  import Hskonfidence.Token

  newtype Parser a = Parser ([Token] -> [(a, [Token])])
  eval (Parser pf) = pf

  instance Monad Parser where
  --return :: a -> Parser a
  --Wraps a value into the closure of a parser function
    return a = Parser (\ts -> [(a, ts)])

  --(>>=)  :: Parser a -> (a -> Parser b) -> Parser b
  --Maps a second parser onto all the results of a first parser.
    p1 >>= p2fn 
      = Parser (\input -> concat 
        [eval (p2fn p1result) input' | (p1result, input') <- eval p1 input ])

  instance MonadPlus Parser where
  --mzero :: Parser a
  --Returns a parser which always fails to parse input
    mzero     = Parser (\ts -> [])

  --mplus :: Parser a -> Parser a -> Parser a
  --Returns a parser which returns the results of both parsers
  --it sums.
    mplus p q = Parser (\ts -> 
      eval p ts ++ eval q ts)

  (<|>) :: Parser a -> Parser a -> Parser a
  left <|> right = Parser (\cs -> case eval (mplus left right) cs of
                                    []      -> []
                                    (x:xs)  -> [x])
