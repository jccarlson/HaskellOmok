----------------------------------------------------------
-- Omok Haskel Project
-- Module       :  HaskellOmok.Main
-- Authors      :  Jason Carlson
--              :  Aldo Perez
--Omok game that is played with two players where the objective
--is to get five pieces in a row, while attempting to block your
--opponent from getting five in a row. This game is typically played
--on a board that is 15 by 15. The program reads from user an x and y
--coordinates and places them on the board and checks for winner after
--each turn.
----------------------------------------------------------

module Main where

--Imports
import Board
import Data.Char
import System.IO
import System.Exit

main = do
    putStrLn("============================\n\t\tOMOK"
        ++"\n============================")
    putStrLn "\n\nIntializing board with size of 15"
    let gb = mkBoard 15
    putStrLn (boardToStr playerToChar gb)
    play gb
{-
play bd

Recursive function that reads x and y from players and places their place
on the board. After the end of each turn, it checks to see if sombody won.
-}
play bd = do
    c <- readXY bd mkPlayer
    let board1 = mark (fst c) (snd c) bd mkPlayer 
    putStrLn (boardToStr playerToChar board1)
    verifyWinner board1 mkPlayer
    d <- readXY board1 mkOpponent
    let board2 = mark (fst d) (snd d) board1 mkOpponent 
    putStrLn (boardToStr playerToChar board2)
    verifyWinner board2 mkOpponent
    play board2

{-
readXY bd p

Reads a 1-based pair of indices (x, y) for player p, denoting an 
unmarked place in a board bd. The function reads inputs from the
standard input (stdin) and returns an Int tuple. Also checks for 
validity of coordinates.
-}
--readXY:: a -> b -> IO(Int, Int)
readXY bd p = do
    putStrLn ("Player " ++ (charToString p :: String))
    putStr "Enter your x coordinate: "
    x <- getX
    putStr "Enter your y coordinate: "
    y <- getX
    --Checking if valid
    if isEmpty x y bd
    then return (x,  y)
    else do
        putStrLn "\n\nAlready Played, Try again!\n\n"
        readXY bd p

getX = do
    putStrLn "Enter a positive value?"
    line <- getLine
    let parsed = reads line :: [(Int, String)] in
        if length parsed == 0
        then getX'
        else 
            let (x, _) = head parsed in
                if x == -1 
                then exitProgram
                else if (x > 0) && (x < 16)
                then return x
                else getX'
    where getX' = do
          putStrLn "Invalid input!"
          getX

{-
playerToChar p

Return a character representation of a player p. It returns a Char
value. This function may be used to print the current state of a 
board 
-}
playerToChar:: Char -> Char
playerToChar p = if p == 'X' then 'X' else if p == 'O' then 'O' else '-'

charToString :: Char -> String
charToString d = [d]

{-
verifyWinner bd p

This function checks to see if the game is won or is a stalemate, 
and returns a boolean
-}
verifyWinner bd p = do
    if (isWonBy bd p)
    then do
        putStrLn("+++++++++++++++++++++++++++++++++")
        putStrLn ("\tPlayer " ++ (charToString p :: String) ++ " Won!!")
        putStrLn ("+++++++++++++++++++++++++++++++++\n\n")
        main
    else if (isDraw bd)
    then do
        putStrLn "Its a draw!\n\n"
        main
    else return()

exitProgram = do
    putStrLn "----------------------------\nThank you for playing"
    putStrLn "-----------------------------"
    exitFailure