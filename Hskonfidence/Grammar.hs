module Hskonfidence.Grammar
  where
  data Program = 
    Program         [Declaration] [Statement]
    deriving (Show)

  data Declaration =
    Declaration     Type Identifier
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
    ArrayType       (Maybe Int) Type  
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
    SimpleExpr      (Maybe UnaryOp) Term [(AddOp, Term)]
    deriving (Show)

  data Term =
    Term            Factor [(MulOp, Factor)]
    deriving (Show)

  data Factor =
    FactorINTLIT          Int         |
    FactorFLOLIT          Float       |
    FactorCHARLIT         Char        |
    FactorSTRLIT          String      |
    FactorDesignator      Designator  |
    FactorExpression      Expression  |
    FactorNotOp           NotOp Factor
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
