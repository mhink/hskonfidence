module Hskonfidence.Grammar
  where

  import Data.Word

  data Program = 
    Program         [Declaration] [Statement]
    deriving (Show, Eq)

  data Declaration =
    Declaration     Type Identifier
    deriving (Show, Eq)

  data Statement =
    Input           Designator              |
    Output          [Expression]            |
    Assignment      Designator Expression   |
    If              Expression [Statement]  |
    While           Expression [Statement]
    deriving (Show, Eq)

  data Type = 
    SimpleInt                               |
    SimpleFloat                             |
    SimpleChar                              |
    ArrayType       (Maybe Int) Type        |
    SimpleBoolean                           |
    SimpleString                            |
    EvaluationFailure String
    deriving (Show, Eq)

  data Identifier =
    Identifier      String
    deriving (Show, Eq)

  data Designator =
    Designator      Identifier              |
    ArrayDesignator Identifier Expression
    deriving (Show, Eq)

  data Expression =
    Expression      SimpleExpr [(RelOp, SimpleExpr)]
    deriving (Show, Eq)

  data SimpleExpr =
    SimpleExpr      (Maybe UnaryOp) Term [(AddOp, Term)]
    deriving (Show, Eq)

  data Term =
    Term            Factor [(MulOp, Factor)]
    deriving (Show, Eq)

  data Factor =
    FactorINTLIT          [Word8]     |
    FactorFLOLIT          [Word8]     |
    FactorCHARLIT         [Word8]     |
    FactorSTRLIT          [Word8]     |
    FactorDesignator      Designator  |
    FactorExpression      Expression  |
    FactorNotOp           NotOp Factor
    deriving (Show, Eq)

  data NotOp    = Not
    deriving (Show, Eq)

  data UnaryOp  = Neg
    deriving (Show, Eq)

  data RelOp    = Equal | Unequal | Less | LessEqual | Greater | GreaterEqual
    deriving (Show, Eq)

  data AddOp    = Plus | Minus | Or
    deriving (Show, Eq)

  data MulOp    = Times | Div | FDiv | Mod | And
    deriving (Show, Eq)
