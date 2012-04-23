###Hskonfidence: A Haskell interpreter for “Confidence?”
Hskonfidence is an interpreter written in Haskell for the Confidence? programming language, as specified at http://www.cse.msstate.edu/~cse4713/termProject/4713_Lab1_Spring_2012.pdf by the students of CSE 4713 at Mississippi State University. Version 1.0.0 currently supports all required functionality.
###Dependencies
Hskonfidence has been tested using the Glasgow Haskell Compiler, version 7.0.4 on Mac OS X Lion (Mac OS 10.7.2) using the Bash shell, and on Windows 7 Professional (Version 6.1).  It is distributed as a Cabal package.  (Cabal is the standard package manager distributed with the Haskell Platform.)
The Haskell Platform may be obtained from http://hackage.haskell.org/platform. 
###Compilation and execution
Please note that the executable is NOT included with the distribution.
runhaskell Setup.hs build will run the Cabal build script.
The resulting distribution will be contained in ./dist/build/hskonf.
When executed, the interpreter will prompt for the path to a valid confidence? source file, or 'q' to quit.
###Code and Test Cases
The following is a listing of all source files included in the package:  

+ Main.hs
+ Setup.hs
+ hskonfidence.cabal
+ README.md
+ /tests
    + lexerTests.cf
    + interpreterTest1.cf
    + interpreterTest2.cf
+ /Hskonfidence
    + /Parser
        + EBNFParsers.hs
        + GrammarParsers.hs
        + RealParser.hs
    + Lexer.hs
    + Token.hs
    + Parser.hs
    + Grammar.hs
    + Interpreter.hs
