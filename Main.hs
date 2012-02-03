import System.Environment
import Hskonfidence.Token
import Hskonfidence.Lexer

main :: IO ()
main = 
  getArgs >>= print . addString . head

addString s = "Hello, " ++ s
