----------------------------------------------------------
-- Omok Haskel Project
-- Module       :  HaskellOmok.Main
-- Authors      :  Jason Carlson
--              :  Aldo Perez
----------------------------------------------------------

module Main where

--Imports
import Board
import System.IO


main :: IO ()
main = do
    putStrLn "Welcome to Omok game\n\nIntializing board with size of 15"
    let gb = mkBoard 15
    putStrLn (boardToStr playerToChar gb)
    --let board1 = mark 1 1 gb mkPlayer 
    --putStrLn (boardToStr playerToChar board1)
    --let board2 = mark 14 14 board1 mkOpponent 
    --putStrLn (boardToStr playerToChar board2)
    --play gb
    --c <- readXY
    --putStrLn ((fst c) ++ (snd c)) 

--play :: IO()
--play bd = do

{-
readXY bd p

Read a 1-based pair of indices (x, y) for player p, denoting an 
unmarked place in a board bd. The function reads inputs from the
standard input (stdin) and returns an IO value such as IO(Int,Int)
or IO(Integer,Integer).
-}
--readXY:: a -> b -> IO(Int, Int)
readXY bd p = do
    putStr "Enter your x coordinate: "
    x <- getLine
    putStr "Enter your y coordinate: "
    y <- getLine
    printCords x y where printCords x y = putStrLn("You picked x: " ++ x ++ " and y: " ++ y )
    Return ()
    --return (x, y)


{-
playerToChar p

Return a character representation of a player p. It returns a Char
value. This function may be used to print the current state of a 
board 
-}
playerToChar:: Char -> Char
playerToChar p = if p == 'X' then 'X' else if p == 'O' then 'O' else '-'

