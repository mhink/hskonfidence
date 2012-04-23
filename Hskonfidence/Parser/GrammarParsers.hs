module Hskonfidence.Parser.GrammarParsers where

import Hskonfidence.Parser.RealParser
import Hskonfidence.Parser.EBNFParsers
import Hskonfidence.Grammar
import Hskonfidence.Token

program :: Parser Program
program =
  do  declarations' <- declarations
      statements'   <- statements
      return (Program declarations' statements')

declaration :: Parser Declaration
declaration =
  do  datatype'   <- datatype
      id          <- string IDENT
      token TERM
      return (Declaration datatype' id)

datatype :: Parser Datatype
datatype =
  simple_type <|>
  array_type

simple_type :: Parser Datatype
simple_type =
  do {token INT;    return HskInt16}  <|>
  do {token FLOAT;  return HskFloat}  <|>
  do {token CHAR;   return HskChar}

array_type :: Parser Datatype
array_type =
  do  token ARRAY
      datatype' <- simple_type
      sizes    <- many array_size
      return (HskArray sizes datatype')

array_size :: Parser Int 
array_size =
  do  token LBRACK
      intlit' <- string INTLIT
      token RBRACK
      return (read intlit' :: Int)

statement :: Parser Statement
statement =
  input_stmt        <|>
  output_stmt       <|>
  assignment_stmt   <|>
  if_stmt           <|>
  while_stmt

input_stmt :: Parser Statement
input_stmt =
  do  token READ
      token LPAREN
      designator' <- designator
      token RPAREN
      token TERM
      return (Input designator')

output_stmt :: Parser Statement
output_stmt =
  do  token WRITE
      token LPAREN
      expressions' <- expressions_separated_by_commas
      token RPAREN
      token TERM
      return (Output expressions')
      
assignment_stmt :: Parser Statement
assignment_stmt =
  do  designator' <- designator
      token ASSIGN
      expression' <- expression
      token TERM
      return (Assignment designator' expression')

if_stmt :: Parser Statement
if_stmt =
  do  token MAYBE
      token LPAREN
      expression' <- expression
      token RPAREN
      token LBRACE
      statements' <- statements
      token RBRACE
      return (If expression' statements')

while_stmt :: Parser Statement
while_stmt =
  do  token WHILST
      token LPAREN
      expression' <- expression
      token RPAREN
      token LBRACE
      statements' <- statements
      token RBRACE
      return (While expression' statements')

designator :: Parser Designator
designator =
  do  ident'    <- string IDENT
      indices'  <- indices
      return (Designator ident' indices')

index :: Parser Expression
index =
  do  token LBRACK
      expression' <- expression
      token RBRACK
      return expression'
      
expression :: Parser Expression
expression =
  do (first, rest) <- simple_expr `separated_with` relational_op
     return (Expression first rest)
     
relational_op :: Parser RelOp
relational_op = do {token TEQ;  return Equal}         <|>
                do {token NE;   return Unequal}       <|>
                do {token TLT;  return Less}          <|>
                do {token LE;   return LessEqual}     <|>
                do {token TGT;  return Greater}       <|>
                do {token GE;   return GreaterEqual}

simple_expr :: Parser SimpleExpr
simple_expr =
  do unary_op' <- zro unary_op
     (first, rest) <- term `separated_with` add_op
     return (SimpleExpr unary_op' first rest)

unary_op :: Parser UnaryOp
unary_op = do {token NEG; return Neg}

add_op :: Parser AddOp
add_op =  do {token PLUS;   return Plus}    <|>
          do {token MINUS;  return Minus}  <|>
          do {token OR;     return Or}

term :: Parser Term
term =
  do (first, rest) <- factor `separated_with` mult_op
     return (Term first rest)

mult_op :: Parser MulOp
mult_op = do {token TIMES;  return Times}  <|> 
          do {token DIV;    return Div}    <|>
          do {token FDIV;   return FDiv}   <|>
          do {token MOD;    return Mod}    <|>
          do {token AND;    return And}

factor :: Parser Factor
factor =
  intlit      <|>
  flolit      <|>
  chrlit      <|>
  strlit      <|>
  fdesignator <|>
  fexpression <|>
  fnot

intlit :: Parser Factor
intlit = do intlit' <- string INTLIT
            return (FactorINTLIT intlit')

flolit :: Parser Factor
flolit = do flolit' <- string FLOLIT
            return (FactorFLOLIT flolit')
            
chrlit :: Parser Factor
chrlit = do chrlit' <- string CHRLIT
            return (FactorCHRLIT chrlit')
            
strlit :: Parser Factor
strlit = do strlit' <- string STRLIT
            return (FactorSTRLIT strlit')
fdesignator :: Parser Factor
fdesignator = do designator' <- designator
                 return (FactorDesignator designator')
                 
fexpression :: Parser Factor
fexpression = do  token LPAREN
                  expression' <- expression
                  token RPAREN
                  return (FactorExpression expression')

fnot :: Parser Factor
fnot = do token NOT
          factor' <- factor
          return (FactorNotOp Not factor')
        
declarations = many declaration
statements = many statement
expressions_separated_by_commas = (expression `separated_by` (token COMMA))
indices = many index
