import System.Environment
import Hskonfidence.Lexer
import Hskonfidence.Parser
import Hskonfidence.Interpreter
import System.IO

main :: IO ()
main = do
  s <- hGetContents stdin
  (interpret . fst . head . eval (Hskonfidence.Parser.program) . lexer) s
