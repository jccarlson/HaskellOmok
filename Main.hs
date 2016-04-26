-----------------------------------------------------------------------------
--
-- module       :  HaskellOmok.Main
-- Authors		:  Jason Carlson, Aldo Perez
-----------------------------------------------------------------------------

module Main where

--Imports
import Board
import System.IO


main :: IO ()
main = do
	putStrLn "Enter your name"
	name <- getLine
	sayHello name

sayHello name = putStrLn("Hi " ++ name ++ "!")


{-
readXY bd p

Read a 1-based pair of indices (x, y) for player p, denoting an 
unmarked place in a board bd. The function reads inputs from the
standard input (stdin) and returns an IO value such as IO(Int,Int)
or IO(Integer,Integer).
-}
readXY bd p = False

{-
playerToChar p

Return a character representation of a player p. It returns a Char
value. This function may be used to print the current state of a 
board 
-}
playerToChar p = False

