import System.Environment
import Hskonfidence.Lexer
import Hskonfidence.Parser
import System.IO

main :: IO ()
main = do
  s <- hGetContents stdin
  (putStrLn . show . fst . head . eval (program) . lexer) s
