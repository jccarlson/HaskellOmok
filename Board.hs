-----------------------------------------------------------------------------
--
-- module       :  HaskellOmok.Board
-- Authors		:  Jason Carlson, Aldo Perez
-----------------------------------------------------------------------------


module Board(
mkBoard,
mkPlayer,
mkOpponent,
size,
row,
column,
mark,
isEmpty,
isMarked,
isMarkedBy,
marker,
isFull,
isWonBy,
isDraw,
isGameOver,
boardToStr) where

{-|
	 Return an empty nxn board, where n is a positive number. A 1-based 
     pair of indices (x,y) will be used to access a specific place of
     the board, where x and y are column and row indices, respectively.
     However, it's up to you how to represent an omok board concretely.
-}
mkBoard :: Int -> [Char]
mkBoard n   |   n <= 0      =   []
            |   otherwise   =   getEmptyListOfLength(n*n)

getEmptyListOfLength :: Int -> [Char]
getEmptyListOfLength 1 = [' ']
getEmptyListOfLength n = (' ':getEmptyListOfLength(n-1))

{-|
	Create and return the first player.
-}
mkPlayer :: Char
mkPlayer = 'X'

{-|
	Create and return the second player, i.e., the opponent.
-}
mkOpponent :: Char
mkOpponent = 'O'

{-|
	Return the size of a board bd, n for an nxn board.
-}
size :: [Char] -> Int
size bd = (floor . sqrt . fromIntegral . length) bd

{-|
	Return a row y of a board bd, where y is a 1-based index. It returns
    a list of size n, where n is the size of bd.
-}
row :: Int -> [Char] -> [Char]
row y bd   |   y < 1           =   []
           |   y > (size bd)   =   []
           |   otherwise       =   getSubList start (start + (size bd) - 1) bd
           where start = (y - 1) * (size bd)

getSubList :: Int -> Int -> [a] -> [a]
getSubList 0     0    (h:l) = [h]
getSubList 0     end  (h:l) = (h:(getSubList 0 (end - 1) l))
getSubList start end  (h:l) = getSubList (start - 1) (end - 1) l

{-|
	Return a column x of a board bd, where x is a 1-based index. It
    returns a list of size n, where n is the size of bd.
-}
column :: Int -> [Char] -> [Char]
column _ []                       =   []
column x bd   |   x < 1           =   []
              |   x > (size bd)   =   []
              |   otherwise       =   getEveryNthElem (size bd) (drop (x - 1) bd)

getEveryNthElem :: Int -> [a] -> [a]
getEveryNthElem _ [] = []
getEveryNthElem n l  = ((head l):(getEveryNthElem n (drop n l)))

{-|
	Mark a place (x,y) in a board bd by a player p, where x and y 
    are 1-based column and row indices. The specified place is assumed
    to be empty (see below).
-}
mark :: Int -> Int -> [Char] -> Char -> [Char]
mark x y bd pl = firstHalf ++ [pl] ++ lastHalf
               where (firstHalf, (_:lastHalf)) = splitAt ((((y - 1) * (size bd)) + (x - 1))) bd 

{-|
	Is a place (x,y) of a board bd unmarked or a stone not placed? 
    The x and y are 1-based column and row indices.
-}
isEmpty :: Int -> Int -> [Char] -> Bool
isEmpty x y bd = ((marker x y bd) == ' ')
{-|
	Does a place (x,y) of a board bd have a stone placed? The x and y 
    are 1-based column and row indices.
-}
isMarked :: Int -> Int -> [Char] -> Bool
isMarked x y bd = not (isEmpty x y bd)

{-|
	Does a place (x,y) of a board bd have a stone placed by a player p?
    The x and y are 1-based column and row indices.
-}
isMarkedBy :: Int -> Int -> [Char] -> Char -> Bool
isMarkedBy x y bd pl = ((marker x y bd) == pl)             

{-|
	Return the player of the stone placed on a place (x,y) of a board 
    bd. The x and y are 1-based column and row indices.
-}
marker :: Int -> Int -> [Char] -> Char
marker x y bd   |  (((y - 1) * (size bd)) + (x - 1)) < 0            = ' '
                |  (((y - 1) * (size bd)) + (x - 1)) >= (length bd) = ' ' 
                |  otherwise                                        = head lastHalf
                where lastHalf = drop ((((y - 1) * (size bd)) + (x - 1))) bd

{-|
	Are all places of board bd marked?
-}
isFull :: [Char] -> Bool
isFull []        = True
isFull (' ':rem) = False 
isFull (_:rem)   = isFull rem

{-|
	Is the game played on a board bd won by a player p?
-}
isWonBy :: [Char] -> Char -> Bool
isWonBy bd pl = checkRemainingPlaces bd pl ((size bd) * (size bd))

checkRemainingPlaces :: [Char] -> Char -> Int -> Bool
checkRemainingPlaces bd pl 0            = False
checkRemainingPlaces bd pl numRemaining = (checkSequence bd pl x y 1 0 5) || (checkSequence bd pl x y 0 1 5) || (checkSequence bd pl x y 1 1 5) || (checkSequence bd pl x y 1 (-1) 5) || (checkRemainingPlaces bd pl (numRemaining - 1))
                                        where x = (mod (numRemaining - 1)  (size bd)) + 1
                                              y = (div (numRemaining - 1)  (size bd)) + 1

checkSequence :: [Char] -> Char -> Int -> Int -> Int -> Int -> Int -> Bool
checkSequence bd pl x y dx dy 0   = True
checkSequence bd pl x y dx dy rem = (isMarkedBy x y bd pl) && (checkSequence bd pl (x + dx) (y + dy) dx dy (rem - 1))

{-|
	Is the game played on a board bd ended in a draw?
-}
isDraw :: [Char] -> Bool
isDraw bd = (isFull bd) && (not (isWonBy bd 'X')) && (not (isWonBy bd 'O'))

{-|
	Is the game played on a board bd over?
-}
isGameOver :: [Char] -> Bool
isGameOver bd = (isDraw bd) || (isWonBy bd 'X') || (isWonBy bd 'O')

{-|
	Return a string representation of a board bd. It is a higher-order
    function, and playerToChar is a function that converts a player 
    to a character representation, e.g., 'O' and 'X'.
-}
boardToStr :: (Char -> Char) -> [Char] -> [Char]
boardToStr playerToChar bd = allRowsToStrings playerToChar bd (size bd)

allRowsToStrings :: (Char -> Char) -> [Char] -> Int -> [Char]
allRowsToStrings playerToChar bd 0       = []
allRowsToStrings playerToChar bd currRow = (allRowsToStrings playerToChar bd (currRow - 1)) ++ (rowToString playerToChar (row currRow bd)) ++ ['\n'] 

rowToString :: (Char -> Char) -> [Char] -> [Char]
rowToString _            []      = []
rowToString playerToChar (h:rem) = [(playerToChar h)] ++ [' '] ++ (rowToString playerToChar rem)