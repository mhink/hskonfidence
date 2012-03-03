module Hskonfidence.Parser
  (
    eval
  ) where
  
  import Data.Char
  import Data.Either
  import Data.Maybe
  import Control.Monad
  import Hskonfidence.Token

  data Program = 
    Program       [Declaration] [Statement]
    deriving (Show)

  data Declaration =
    Declaration   Type Identifier
    deriving (Show)

  data Statement =
    Input           Designator              |
    Output          [Expression]            |
    Assignment      Designator Expression   |
    If              Expression [Statement]  |
    While           Expression [Statement]
    deriving (Show)

  data Type = 
    SimpleInt                               |
    SimpleFloat                             |
    SimpleChar                              |
    ArrayType       Int Type  
    deriving (Show)

  data Identifier =
    Identifier      String
    deriving (Show)

  data Designator =
    Designator      Identifier              |
    ArrayDesignator Identifier Expression
    deriving (Show)

  data Expression =
    Expression      SimpleExpr [(RelOp, SimpleExpr)]
    deriving (Show)

  data SimpleExpr =
    SimpleExpr      UnaryOp Term [(AddOp, Term)]
    deriving (Show)

  data Term =
    Term            Factor [(MulOp, Factor)]
    deriving (Show)

  data Factor =
    INTLIT          Int         |
    FLOLIT          Float       |
    CHARLIT         Char        |
    STRLIT          String      |
    DesFactor       Designator  |
    ExprFactor      Expression  |
    NotFactor       NotOp Factor
    deriving (Show)
    

  data NotOp    = Not
    deriving (Show)

  data UnaryOp  = Neg
    deriving (Show)

  data RelOp    = Equal | Unequal | Less | LessEqual | Greater | GreaterEqual
    deriving (Show)

  data AddOp    = Plus | Minus | Or
    deriving (Show)

  data MulOp    = Times | Div | FDiv | Mod | And
    deriving (Show)

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

  item :: Parser Token
  --Returns a parser which consumes the first token of a token array
  --Fails if the input array is empty.
  item = Parser (\ts -> case ts of
                          [] -> []
                          (t:ts) -> [(t, ts)])
