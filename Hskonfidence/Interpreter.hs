module Hskonfidence.Interpreter
  where

  -- IMPORT-- Import statements
  import qualified Data.Binary          as Binary
  import qualified Data.ByteString.Lazy as ByteString
  import qualified Data.Map             as Map
  import qualified Hskonfidence.Lexer   as HSL
  import qualified Hskonfidence.Parser  as HSP

  import Control.Monad.State
  import Data.Array
  import Data.Int
  import Data.List
  import Data.Maybe
  import Data.Word
  import Hskonfidence.Grammar

  -- TYPES -- Type declarations
  type Reference      = (Type, MemAddress)
  type MemAddress     = Int

  type SymbolTable    = Map.Map String Reference
  type Memory         = Array MemAddress Word8

  type IState         = (SymbolTable, Memory)
  type Interpreter a  = StateT IState IO a

  -- DATA -- Data declarations
  data ExpressionResult =
    SimpleInt16Data   Int16   |
    SimpleFloatData   Float   |
    SimpleCharData    Char    | 
    SimpleStringData  String  |
    SimpleBoolData    Bool    |
    Error             String
    deriving (Show)

  --INTERPRETER -- Interpreter actions
  --Initializes the interpreter and begins interpretation.
  interpret :: Program -> IO ()
  interpret prog =
    do let st   = Map.empty
       let mem  = array (0, memSize) [(i,0) | i <- [0..memSize]]
       evalStateT (program prog) (st, mem)

  --Builds symbol table, executes statements
  program :: Program -> Interpreter ()
  program (Program declarations statements) =
    do console "Interpreting program."
       createSymbolTableFrom declarations
       executeAll statements

  -- Actually builds and returns a symbol table
  createSymbolTableFrom :: [Declaration] -> Interpreter ()
  createSymbolTableFrom declarations =
    do console "Creating symbol table"
       ( _ , mem) <- get
       let st = (Map.fromList . snd . Data.List.mapAccumL macc 0) declarations
       put (st, mem)

  -- Map an execute action over a list of statements
  executeAll :: [Statement] -> Interpreter ()
  executeAll statements = mapM_ execute statements

  -- Execute different types of statements
  execute :: Statement -> Interpreter ()
  execute (Assignment designator expression) =
    do (dtype, addr)    <- symbolTableLookup designator
       expressionResult <- evaluate expression
       if (dtype == (typeOf expressionResult))
         then writeMemory addr (size dtype) (getBytes expressionResult)
         else fail "Type mismatch in assignment statement"

  execute (Output expressions) =
    do results <- evaluateAll expressions
       mapM_ (\r -> console r) results

  -- Map an evaluation over several expressions
  evaluateAll :: [Expression] -> Interpreter [ExpressionResult]
  evaluateAll expressions = mapM evaluate expressions

  evaluate :: Expression -> Interpreter ExpressionResult
  evaluate (Expression simple relOpExprs) =
    do res1 <- evalSimple simple
       foldM (combineSimpleExpression) res1 relOpExprs

  evalSimple :: SimpleExpr -> Interpreter ExpressionResult
  evalSimple (SimpleExpr unary term addOpTerms) =
    do res1 <- evalTerm term
       foldM (combineTerm) res1 addOpTerms

  evalTerm :: Term -> Interpreter ExpressionResult
  evalTerm (Term factor mulOpFactors) =
    do res1 <- evalFactor factor
       foldM (combineFactor) res1 mulOpFactors 

  evalFactor :: Factor -> Interpreter ExpressionResult
  evalFactor (FactorDesignator designator) =
    do (datatype, addr) <- symbolTableLookup designator
       bytes <- readMemory addr datatype 
       return (getFromBytes datatype bytes)

  evalFactor (FactorExpression expression) =
    do evaluate expression

  evalFactor factor = 
    do case factor of
         FactorINTLIT val   -> return (getFromBytes SimpleInt val)
         FactorFLOLIT val   -> return (getFromBytes SimpleFloat val)
         FactorCHARLIT val  -> return (getFromBytes SimpleChar val)
  
  -- COMB -- Combination functions

  combineSimpleExpression :: ExpressionResult -> (RelOp, SimpleExpr) -> Interpreter ExpressionResult
  combineSimpleExpression rse1 (op, se2) =
    do rse2 <- evalSimple se2
       return ((relop op) rse1 rse2)

  combineFactor :: ExpressionResult -> (MulOp, Factor) -> Interpreter ExpressionResult
  combineFactor rf1 (op, f2) =
    do rf2 <- evalFactor f2
       return ((mulop op) rf1 rf2)

  combineTerm :: ExpressionResult -> (AddOp, Term) -> Interpreter ExpressionResult
  combineTerm rt1 (op, t2) =
    do rt2 <- evalTerm t2
       return ((addop op) rt1 rt2)

  hskand (SimpleBoolData b1) (SimpleBoolData b2)
    = SimpleBoolData (and [b1, b2])

  hskand _ _ = Error "Undefined operation (and)"

  hskor (SimpleBoolData b1) (SimpleBoolData b2)
    = SimpleBoolData (or [b1, b2])

  hskor _ _ = Error "Undefined operation (or)"

  greaterequal (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleBoolData (i1 >= i2)

  greaterequal (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleBoolData (f1 >= f2)

  greaterequal (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleBoolData (f1 >= f2)

  greaterequal (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleBoolData (f1 >= f2)

  greaterequal _ _ = Error "Undefined operation (greaterequal)"

  greater (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleBoolData (i1 > i2)

  greater (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleBoolData (f1 > f2)

  greater (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleBoolData (f1 > f2)

  greater (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleBoolData (f1 > f2)

  greater _ _ = Error "Undefined operation (greater)"

  lessequal (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleBoolData (i1 <= i2)

  lessequal (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleBoolData (f1 <= f2)

  lessequal (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleBoolData (f1 <= f2)

  lessequal (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleBoolData (f1 <= f2)

  lessequal _ _ = Error "Undefined operation (lessequal)"

  less (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleBoolData (i1 < i2)

  less (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleBoolData (f1 < f2)

  less (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleBoolData (f1 < f2)

  less (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleBoolData (f1 < f2)

  less _ _ = Error "Undefined operation (less)"

  unequal (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleBoolData (i1 /= i2)

  unequal (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleBoolData (f1 /= f2)

  unequal (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleBoolData (f1 /= f2)

  unequal (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleBoolData (f1 /= f2)

  unequal (SimpleBoolData b1) (SimpleBoolData b2)
    = SimpleBoolData (b1 /= b2)

  unequal _ _ = Error "Undefined operation (unequal)"

  equal (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleBoolData (i1 == i2)

  equal (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleBoolData (f1 == f2)

  equal (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleBoolData (f1 == f2)

  equal (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleBoolData (f1 == f2)

  equal (SimpleBoolData b1) (SimpleBoolData b2)
    = SimpleBoolData (b1 == b2)

  equal _ _ = Error "Undefined operation (equal)"

  minus (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleInt16Data (i1 - i2)

  minus (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleFloatData (f1 - f2)

  minus (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleFloatData (f1 - f2)

  minus (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleFloatData (f1 - f2)

  minus _ _ = Error "Undefined operation (minus)"

  plus (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleInt16Data (i1 + i2)

  plus (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleFloatData (f1 + f2)

  plus (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleFloatData (f1 + f2)

  plus (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleFloatData (f1 + f2)

  plus _ _ = Error "Undefined operation (plus)"

  fdiv (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = let f1 = fromIntegral i1 :: Float
          f2 = fromIntegral i2 :: Float
      in SimpleFloatData (f1 / f2)

  fdiv (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleFloatData (f1 / f2)

  fdiv (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleFloatData (f1 / f2)

  fdiv (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleFloatData (f1 / f2)

  fdiv _ _ = Error "Undefined operation (fdiv)"

  idiv (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleInt16Data (i1 `div` i2)

  idiv (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleFloatData (f1 / f2)

  idiv (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleFloatData (f1 / f2)

  idiv (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleFloatData (f1 / f2)

  idiv _ _ = Error "Undefined operation (idiv)" 

  times (SimpleInt16Data i1) (SimpleInt16Data i2) 
    = SimpleInt16Data (i1 * i2)

  times (SimpleFloatData f1) (SimpleFloatData f2) 
    = SimpleFloatData (f1 * f2)

  times (SimpleInt16Data i1) (SimpleFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in SimpleFloatData (f1 * f2)

  times (SimpleFloatData f1) (SimpleInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in SimpleFloatData (f1 * f2)

  times _ _ = Error "Undefined operation (times)"

  mulop Times = times
  mulop Div   = idiv
  mulop FDiv  = fdiv
  mulop And   = hskand

  addop Plus  = plus
  addop Minus = minus
  addop Or    = hskor

  relop Equal = equal
  relop Unequal = unequal
  relop Less = less
  relop LessEqual = lessequal
  relop Greater = greater
  relop GreaterEqual = greaterequal

  -- CONV -- Convenience functions
  -- map-accumulate helper function
  macc :: MemAddress -> Declaration -> (MemAddress, (String, Reference))
  macc addr (Declaration datatype (Identifier idstring)) =
    ((addr + size datatype), (idstring, (datatype, addr)))

  -- Gets the memory size.
  memSize :: Int
  memSize = 1024

  -- Gets the size of a datatype
  size :: Type -> Int
  size SimpleInt = 2
  size SimpleFloat = 13
  size SimpleChar = 1
  size _ = 0

  --Given a data container, resolves its type
  typeOf :: ExpressionResult -> Type 
  typeOf (SimpleInt16Data _ ) = SimpleInt
  typeOf (SimpleFloatData _ ) = SimpleFloat
  typeOf (SimpleCharData  _ ) = SimpleChar

  getFromBytes :: Type -> [Word8] -> ExpressionResult
  getFromBytes SimpleInt words =
    SimpleInt16Data (Binary.decode (ByteString.pack words) :: Int16)
  getFromBytes SimpleFloat words =
    SimpleFloatData (Binary.decode (ByteString.pack words) :: Float)
  getFromBytes SimpleChar words =
    SimpleCharData (Binary.decode (ByteString.pack words) :: Char)

  getBytes :: ExpressionResult -> [Word8]
  getBytes (SimpleInt16Data val) = getBytes' val
  getBytes (SimpleFloatData val) = getBytes' val
  getBytes (SimpleCharData  val) = getBytes' val

  getBytes' :: Binary.Binary a => (a -> [Word8])
  getBytes' = (ByteString.unpack . Binary.encode)
  
  -- Outputs a showable data type to the console.
  console :: Show a => a -> Interpreter ()
  console a = liftIO $ print a
  
  -- Function for parsing and lexing a string of confidence? source code
  getProgram :: (String -> Program)
  getProgram = (fst . head . HSP.eval (HSP.program) . HSL.lexer)

  -- Writes a set of bytes to memory.
  writeMemory :: MemAddress -> Int -> [Word8] -> Interpreter ()
  writeMemory addr length val =
    do (st, mem) <- get
       let mem' = (mem // [(addr + addr', (val !! addr')) | addr' <- [0..((length)-1)]])
       put (st, mem')

  -- Reads a set of bytes from memory
  readMemory :: MemAddress -> Type -> Interpreter [Word8]
  readMemory addr datatype =
    do (st, mem) <- get
       return [(mem ! addr') | addr' <- [addr..(addr + (size datatype  - 1))]]

  -- Looks up a designator in the symbol table.
  symbolTableLookup :: Designator -> Interpreter Reference
  symbolTableLookup (Designator (Identifier idstring)) =
    do (st, mem) <- get
       case Map.lookup idstring st of
         Nothing  -> fail "Symbol table lookup failed."
         Just ref -> return ref

  -- Tests the functionality of the interpreter.
  test :: IO ()
  test =
    do let fromString = "         \
\        int i1 ?                 \
\        float f1 ?               \
\        i1 is 5 ?                \
\        write ( i1 * 5.0 ) ?     \
\        write ( 2 < 3 ) ?        \
\        write ( 2 and 'c' ) ?       "
       print "Beginning test."
       interpret $ getProgram fromString
