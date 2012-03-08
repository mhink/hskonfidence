module Hskonfidence.Parser
  where 

  import Control.Monad
  import Hskonfidence.Token
  import Hskonfidence.Grammar
  
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

  identifier :: Parser Identifier
  identifier = do Token IDENT str <- (token IDENT)
                  return (Identifier str)

  factor :: Parser Factor
  factor = do { n <- notop;
                f <- factor; 
                return (FactorNotOp n f) }            <|>
           do { des <- designator;
                return (FactorDesignator des) }       <|>
           do { token LPAREN;
                expr <- expression;
                token RPAREN;
                return (FactorExpression expr) }      <|>
           do { Token INTLIT lit <- token INTLIT; 
                return (FactorINTLIT (read lit)) }    <|>
           do { Token FLOLIT lit <- token FLOLIT;
                return (FactorFLOLIT (read lit)) }    <|>
           do { Token CHRLIT lit <- token CHRLIT;
                return (FactorCHARLIT (head lit)) }   <|>
           do { Token STRLIT lit <- token STRLIT;
                return (FactorSTRLIT lit) }


  designator :: Parser Designator
  designator = designator' <|> arraydesignator

  designator' :: Parser Designator
  designator' = do {id <- identifier; return (Designator id) }

  arraydesignator :: Parser Designator
  arraydesignator = do id <- identifier
                       token LBRACK
                       expr <- expression
                       token RBRACK
                       return (ArrayDesignator id expr)

  notop :: Parser NotOp
  notop = do {token NOT; return Not}

  unaryop :: Parser UnaryOp
  unaryop = do {token NEG; return Neg}

  addop :: Parser AddOp
  addop = do {token PLUS;   return Plus}    <|>
          do {token MINUS;  return Minus}  <|>
          do {token OR;     return Or}

  mulop :: Parser MulOp
  mulop = do {token TIMES;  return Times}  <|> 
          do {token DIV;    return Div}    <|>
          do {token FDIV;   return FDiv}   <|>
          do {token MOD;    return Mod}    <|>
          do {token AND;    return And}

  relop :: Parser RelOp
  relop = do {token TEQ;  return Equal}         <|>
          do {token NE;   return Unequal}       <|>
          do {token TLT;  return Less}          <|>
          do {token LE;   return LessEqual}     <|>
          do {token TGT;  return Greater}       <|>
          do {token GE;   return GreaterEqual}

  term :: Parser Term
  term =  do   f <- factor
               fs <- many factors
               return (Term f fs) 
  
  factors :: Parser (MulOp, Factor)
  factors = do {  mo <- mulop;
                  f  <- factor;
                  return (mo, f) }

  simpleexpr :: Parser SimpleExpr
  simpleexpr = do uo <- zro unaryop
                  t <- term
                  ts <- many terms
                  return (SimpleExpr uo t ts)

  simpleexprs :: Parser (RelOp, SimpleExpr)
  simpleexprs = do ro <- relop
                   se <- simpleexpr
                   return (ro, se)

  terms :: Parser (AddOp, Term)
  terms = do ao <- addop
             t  <- term
             return (ao, t)

  expression :: Parser Expression
  expression = do expr  <- simpleexpr
                  exprs <- many simpleexprs
                  return (Expression expr exprs)

  cexpression :: Parser Expression
  cexpression = do token COMMA
                   expr <- expression
                   return expr

  statement :: Parser Statement
  statement = do { token READ;
                   token LPAREN;
                   d <- designator;
                   token RPAREN;
                   token TERM;
                   return (Input d) } <|>

              do { token WRITE;
                   token LPAREN;
                   expr <- expression;
                   exprs <- many cexpression;
                   token RPAREN;
                   token TERM;
                   return (Output (expr:exprs)) } <|>

              do { d <- designator;
                   token ASSIGN;
                   expr <- expression;
                   token TERM;
                   return (Assignment d expr) } <|>

              do { token MAYBE;
                   token LPAREN;
                   expr  <- expression;
                   token RPAREN;
                   token LBRACE;
                   stmts <- many statement;
                   token RBRACE;
                   return (If expr stmts) } <|> 

              do { token WHILST;
                   token LPAREN;
                   expr <- expression;
                   token RPAREN;
                   token LBRACE;
                   stmts <- many statement;
                   token RBRACE;
                   return (While expr stmts) } 

  program :: Parser Program
  program = do decls <- many declaration
               stmts <- many statement
               return (Program decls stmts)

  declaration :: Parser Declaration
  declaration = do ct <- ctype
                   id <- identifier
                   token TERM
                   return (Declaration ct id)

  ctype :: Parser Type
  ctype = do {token INT;    return SimpleInt }    <|>
          do {token FLOAT;  return SimpleFloat }  <|>
          do {token CHAR;   return SimpleChar }   <|>
          do {token ARRAY;
              ct <- ctype;
              int <- zro arint;
              return (ArrayType int ct) }

  arint :: Parser Int
  arint = do token LBRACK
             Token INTLIT int <- token INTLIT
             token RBRACK
             return (read int)
