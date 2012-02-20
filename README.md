# hskonfidence
## A Haskell Interpreter for 'Confidence?'
#### Author: Matt Hink
#### Class: CSE4713 Programming Languages
***

##Hskonfidence: A Haskell interpreter for “Confidence?”
Hskonfidence is an interpreter written in Haskell for the Confidence? programming language, as specified at http://www.cse.msstate.edu/~cse4713/termProject/4713_Lab1_Spring_2012.pdf by the students of CSE 4713 at Mississippi State University.  The current version only supports lexical analysis of source files, with parsing and interpretation to be implemented in later versions.
###Dependencies
Hskonfidence has been tested using the Glasgow Haskell Compiler, version 7.0.4 on Mac OS X Lion (Mac OS 10.7.2) using the Bash shell.  It is distributed as a Cabal package.  (Cabal is the standard package manager distributed with the Haskell Platform.)  Several Bash scripts are included; therefore, this submission should be run on a Unix-based platform.
The Haskell Platform may be obtained from http://hackage.haskell.org/platform. 
###Compilation and execution
A set of Bash scripts are provided for compilation and execution.  Please note that the executable is NOT included with the distribution, as it was compiled for Mac OS X and will likely not run on other platforms.
runhaskell Setup.hs build will run the Cabal build script. 
./run.sh sets up a small interactive console into which the user may enter the paths of valid confidence? source files and uses hskonf to perform lexical analysis on them.
The hskonf interpreter operation is documented in README.md.
###Code and Test Cases
The following is a listing of all source files included in the package:
* tests.c?
* Main.hs
* Setup.hs
* hskonfidence.cabal
* README.md
* Hskonfidence/
  * Lexer.hs
  * Token.hs
The test package included with this program is /tests.c?, and demonstrates lexer performance on a variety of valid and invalid lexemes.  Sample test output is included in /testOut.txt
Correct lexer performance was verified on all token types, as well as correct failure on unreadable lexemes.
