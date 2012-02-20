import System.Environment
import Hskonfidence.Token
import Hskonfidence.Lexer
import System.IO

main :: IO ()
main = do
  s <- hGetContents stdin
  (putStrLn . show . lexer) s
