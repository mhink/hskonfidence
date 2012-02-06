module Hskonfidence.Lexer
  (
    lex    
  ) where

  import Hskonfidence.Token

  lex :: String -> [Token]
    --An empty string should return an array consisting only of one EOF token
    lex [] = [ EOF ]

    --A nonempty string should return a list of tokens
    lex xs =
      let ( token, remainingString ) = nextToken xs --parse next token
      in  ( token : lex remainingString )           --construct token array

  nextToken :: String -> ( Token, String )
    --An empty string should return an EOF token and an empty remaining array
    nextToken [] = ( EOF, [] )

    --Strategy: check for complicated tokens first
    --Strategy: then check for simple tokens
