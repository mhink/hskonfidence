import System.Environment
import Hskonfidence.Lexer
import Hskonfidence.Parser
import Hskonfidence.Interpreter
import System.IO

main :: IO ()
main = do
  putStrLn "Welcome to the Hskonfidence interpreter."
  putStrLn "Please enter a path to a valid confidence? source file."
  putStrLn "Enter 'q' to quit."
  interpretFile

interpretFile :: IO () 
interpretFile = do
  fp <- getLine
  if (fp == "q")
    then return ()
    else interpretFile' fp

interpretFile' :: FilePath -> IO ()
interpretFile' fp = do
  source <- catch (readFile fp)
                  (\e -> do let err = show ( e :: IOError)
                            hPutStrLn stdout ( "File not found. Please enter another.")
                            return "" )
  let tokens    = lexer source
  let parseTree = (fst . head . eval(Hskonfidence.Parser.program)) tokens
  interpret parseTree
  interpretFile
