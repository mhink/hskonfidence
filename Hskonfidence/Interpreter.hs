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
  import System.IO.Unsafe

  -- TYPES -- Datatype declarations
  type Reference      = (Datatype, MemAddress)
  type MemAddress     = Int

  type SymbolTable    = Map.Map String Reference
  type Memory         = Array MemAddress Word8

  type IState         = (SymbolTable, Memory)
  type Interpreter a  = StateT IState IO a

  -- DATA -- Data declarations
  data ExpressionResult =
    HskInt16Data   Int16   |
    HskFloatData   Float   |
    HskCharData    Char    | 
    HskStringData  String  |
    HskBoolData    Bool    |
    Error          String
  
  instance Show ExpressionResult where
    show (HskInt16Data  i) = show i
    show (HskFloatData  f) = show f
    show (HskCharData   c) = show c
    show (HskStringData s) = show s
    show (HskBoolData   b) = show b
    show (Error         s) = show s

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
    do createSymbolTableFrom declarations
       executeAll statements
       console "HSK: Program finished."

  -- Actually builds and returns a symbol table
  createSymbolTableFrom :: [Declaration] -> Interpreter ()
  createSymbolTableFrom declarations =
    do ( _ , mem) <- get
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
         else fail "Datatype mismatch in assignment statement"
  
  execute (Input designator) =
    do (dtype, addr) <- symbolTableLookup designator
       res <- readConsole dtype
       writeMemory addr (size dtype) (getBytes res)

  execute (Output expressions) =
    do results <- evaluateAll expressions
       mapM_ (\r -> consolePut r) results
       console ""
       
  execute (If expression statements) =
    do result <- evaluate expression
       case result of
         HskBoolData True  -> executeAll statements
         HskBoolData False -> return ()
         otherwise            -> fail "Maybe-expression must evaluate to boolean"

  execute (While expression statements) =
    do result <- evaluate expression
       case result of
         HskBoolData True  -> do executeAll statements
                                 execute (While expression statements)
         HskBoolData False -> return ()
         otherwise            -> fail "Whilst-expression must evaluate to boolean"

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
         FactorINTLIT val   -> return (HskInt16Data (read val :: Int16))
         FactorFLOLIT val   -> return (HskFloatData (read val :: Float))
         FactorCHRLIT val   -> return (HskCharData  (head val))
         FactorSTRLIT val   -> return (HskStringData val)
  
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

  hskand (HskBoolData b1) (HskBoolData b2)
    = HskBoolData (and [b1, b2])

  hskand _ _ = Error "Undefined operation (and)"

  hskor (HskBoolData b1) (HskBoolData b2)
    = HskBoolData (or [b1, b2])

  hskor _ _ = Error "Undefined operation (or)"

  greaterequal (HskInt16Data i1) (HskInt16Data i2) 
    = HskBoolData (i1 >= i2)

  greaterequal (HskFloatData f1) (HskFloatData f2) 
    = HskBoolData (f1 >= f2)

  greaterequal (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskBoolData (f1 >= f2)

  greaterequal (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskBoolData (f1 >= f2)

  greaterequal _ _ = Error "Undefined operation (greaterequal)"

  greater (HskInt16Data i1) (HskInt16Data i2) 
    = HskBoolData (i1 > i2)

  greater (HskFloatData f1) (HskFloatData f2) 
    = HskBoolData (f1 > f2)

  greater (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskBoolData (f1 > f2)

  greater (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskBoolData (f1 > f2)

  greater _ _ = Error "Undefined operation (greater)"

  lessequal (HskInt16Data i1) (HskInt16Data i2) 
    = HskBoolData (i1 <= i2)

  lessequal (HskFloatData f1) (HskFloatData f2) 
    = HskBoolData (f1 <= f2)

  lessequal (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskBoolData (f1 <= f2)

  lessequal (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskBoolData (f1 <= f2)

  lessequal _ _ = Error "Undefined operation (lessequal)"

  less (HskInt16Data i1) (HskInt16Data i2) 
    = HskBoolData (i1 < i2)

  less (HskFloatData f1) (HskFloatData f2) 
    = HskBoolData (f1 < f2)

  less (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskBoolData (f1 < f2)

  less (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskBoolData (f1 < f2)

  less _ _ = Error "Undefined operation (less)"

  unequal (HskInt16Data i1) (HskInt16Data i2) 
    = HskBoolData (i1 /= i2)

  unequal (HskFloatData f1) (HskFloatData f2) 
    = HskBoolData (f1 /= f2)

  unequal (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskBoolData (f1 /= f2)

  unequal (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskBoolData (f1 /= f2)

  unequal (HskBoolData b1) (HskBoolData b2)
    = HskBoolData (b1 /= b2)

  unequal _ _ = Error "Undefined operation (unequal)"

  equal (HskInt16Data i1) (HskInt16Data i2) 
    = HskBoolData (i1 == i2)

  equal (HskFloatData f1) (HskFloatData f2) 
    = HskBoolData (f1 == f2)

  equal (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskBoolData (f1 == f2)

  equal (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskBoolData (f1 == f2)

  equal (HskBoolData b1) (HskBoolData b2)
    = HskBoolData (b1 == b2)

  equal _ _ = Error "Undefined operation (equal)"

  minus (HskInt16Data i1) (HskInt16Data i2) 
    = HskInt16Data (i1 - i2)

  minus (HskFloatData f1) (HskFloatData f2) 
    = HskFloatData (f1 - f2)

  minus (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskFloatData (f1 - f2)

  minus (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskFloatData (f1 - f2)

  minus _ _ = Error "Undefined operation (minus)"

  plus (HskInt16Data i1) (HskInt16Data i2) 
    = HskInt16Data (i1 + i2)

  plus (HskFloatData f1) (HskFloatData f2) 
    = HskFloatData (f1 + f2)

  plus (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskFloatData (f1 + f2)

  plus (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskFloatData (f1 + f2)

  plus _ _ = Error "Undefined operation (plus)"

  fdiv (HskInt16Data i1) (HskInt16Data i2) 
    = let f1 = fromIntegral i1 :: Float
          f2 = fromIntegral i2 :: Float
      in HskFloatData (f1 / f2)

  fdiv (HskFloatData f1) (HskFloatData f2) 
    = HskFloatData (f1 / f2)

  fdiv (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskFloatData (f1 / f2)

  fdiv (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskFloatData (f1 / f2)

  fdiv _ _ = Error "Undefined operation (fdiv)"

  idiv (HskInt16Data i1) (HskInt16Data i2) 
    = HskInt16Data (i1 `div` i2)

  idiv (HskFloatData f1) (HskFloatData f2) 
    = HskFloatData (f1 / f2)

  idiv (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskFloatData (f1 / f2)

  idiv (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskFloatData (f1 / f2)

  idiv _ _ = Error "Undefined operation (idiv)" 

  times (HskInt16Data i1) (HskInt16Data i2) 
    = HskInt16Data (i1 * i2)

  times (HskFloatData f1) (HskFloatData f2) 
    = HskFloatData (f1 * f2)

  times (HskInt16Data i1) (HskFloatData f2) 
    = let f1 = fromIntegral i1 :: Float
      in HskFloatData (f1 * f2)

  times (HskFloatData f1) (HskInt16Data i2) 
    = let f2 = fromIntegral i2 :: Float
      in HskFloatData (f1 * f2)

  times _ _ = Error "Undefined operation (times)"

  mod' (HskInt16Data i1) (HskInt16Data i2)
    = HskInt16Data (i1 `mod` i2)

  mod' _ _ = Error "Undefined operation (mod)"

  mulop Times = times
  mulop Div   = idiv
  mulop FDiv  = fdiv
  mulop And   = hskand
  mulop Mod   = mod'

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
  macc addr (Declaration datatype idstring) =
    ((addr + size datatype), (idstring, (datatype, addr)))

  -- Gets the memory size.
  memSize :: Int
  memSize = 1024

  -- Gets the size of a datatype
  size :: Datatype -> Int
  size HskInt16 = 2
  size HskFloat = 13
  size HskChar = 1
  size (HskArray sizes datatype) =
    foldl (*) (size datatype) sizes
  size _ = 0

  --Given a data container, resolves its type
  typeOf :: ExpressionResult -> Datatype 
  typeOf (HskInt16Data _ ) = HskInt16
  typeOf (HskFloatData _ ) = HskFloat
  typeOf (HskCharData  _ ) = HskChar

  isInt :: ExpressionResult -> Bool
  isInt er = (typeOf er) == HskInt16

  getFromBytes :: Datatype -> [Word8] -> ExpressionResult
  getFromBytes HskInt16 words =
    HskInt16Data (Binary.decode (ByteString.pack words) :: Int16)
  getFromBytes HskFloat words =
    HskFloatData (Binary.decode (ByteString.pack words) :: Float)
  getFromBytes HskChar words =
    HskCharData (Binary.decode (ByteString.pack words) :: Char)

  getBytes :: ExpressionResult -> [Word8]
  getBytes (HskInt16Data val) = getBytes' val
  getBytes (HskFloatData val) = getBytes' val
  getBytes (HskCharData  val) = getBytes' val

  getBytes' :: Binary.Binary a => (a -> [Word8])
  getBytes' = (ByteString.unpack . Binary.encode)
  
  -- Outputs a showable data type to the console.
  console :: Show a => a -> Interpreter ()
  console a = liftIO $ print a

  consolePut :: Show a => a -> Interpreter ()
  consolePut a = 
    do let toWrite = show a
       liftIO $ putStr toWrite
  
  readConsole :: Datatype -> Interpreter ExpressionResult
  readConsole HskInt16 =
    do str <- liftIO $ getLine
       return (HskInt16Data (read str :: Int16))
       
  readConsole HskFloat =
    do str <- liftIO $ getLine
       return (HskFloatData (read str :: Float))
       
  readConsole HskChar  =
    do chr <- liftIO $ getChar
       return (HskCharData chr)
  
  -- Function for parsing and lexing a string of confidence? source code
  getProgram :: (String -> Program)
  getProgram = (fst . head . HSP.eval (HSP.program) . HSL.lexer)

  -- Writes a set of bytes to memory.
  writeMemory :: MemAddress -> Int -> [Word8] -> Interpreter ()
  writeMemory addr length val =
    do (st , mem) <- get
       let mem' = (mem // [(addr + addr', (val !! addr')) | addr' <- [0..((length)-1)]])
       put (st, mem')

  -- Reads a set of bytes from memory
  readMemory :: MemAddress -> Datatype -> Interpreter [Word8]
  readMemory addr datatype =
    do (_ , mem) <- get
       return [(mem ! addr') | addr' <- [addr..(addr + (size datatype  - 1))]]

  -- Looks up a designator in the symbol table.
  -- TODO: ARRAYS
  symbolTableLookup :: Designator -> Interpreter Reference
  symbolTableLookup (Designator idstring indexExprs) =
    do  (st, _ ) <- get
        indices <- getIndices indexExprs
        case Map.lookup idstring st of
          Nothing   -> fail "HSK: Symbol table lookup failed." 
          Just ((HskArray sizes baseType), addr) -> 
            return (baseType, (adjustedRef sizes indices (size baseType) addr))
          Just ref -> return ref

  adjustedRef :: [Int] -> [Int] -> Int -> Int -> Int
  adjustedRef sizes indices typeSize baseAddr =
    snd $ foldl
      (\((i:is), addr) index -> (is, (addr + ((foldl (*) 1 is) * typeSize * index))))
      (sizes, baseAddr)
      indices

  getIndices :: [Expression] -> Interpreter [Int]
  getIndices indexExpressions =
    do  indices <- evaluateAll indexExpressions
        if any (not . isInt) indices
          then fail "HSK: Non-integer array index expression"
          else return (map (\(HskInt16Data int) -> (fromIntegral int)) indices)

  -- Tests the functionality of the interpreter.
  test :: IO ()
  test =
    do let fromString = "         \
\        int ix ? \
\        array int [ 5 ] ai1 ?                 \
\        i1 [ 0 ] is 5 ?                \
\        write ( i1 [ 1 ]  ) ?  "
       print "Beginning test."
       let prog = getProgram fromString
       print prog
       interpret prog
