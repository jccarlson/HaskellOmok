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

mkBoard :: Int -> [Char]
mkBoard n   |   n <= 0      =   []
            |   otherwise   =   getEmptyListOfLength(n*n)

getEmptyListOfLength :: Int -> [Char]
getEmptyListOfLength 1 = [' ']
getEmptyListOfLength n = (' ':getEmptyListOfLength(n-1))

mkPlayer = 'X'
mkOpponent = 'O'

size :: [Char] -> Int
size bd = (floor . sqrt . fromIntegral . length) bd

row :: Int -> [Char] -> [Char]
row y bd   |   y < 1           =   []
           |   y > (size bd)   =   []
           |   otherwise       =   getSubList start (start + (size bd) - 1) bd
           where start = (y - 1) * (size bd)

getSubList :: Int -> Int -> [a] -> [a]
getSubList 0     0    (h:l) = [h]
getSubList 0     end  (h:l) = (h:(getSubList 0 (end - 1) l))
getSubList start end  (h:l) = getSubList (start - 1) (end - 1) l

column :: Int -> [Char] -> [Char]
column _ []                       =   []
column x bd   |   x < 1           =   []
              |   x > (size bd)   =   []
              |   otherwise       =   getEveryNthElem (size bd) (drop (x - 1) bd)

getEveryNthElem :: Int -> [a] -> [a]
getEveryNthElem _ [] = []
getEveryNthElem n l  = ((head l):(getEveryNthElem n (drop n l)))


mark :: Int -> Int -> [Char] -> Char -> [Char]
mark x y bd pl = firstHalf ++ [pl] ++ lastHalf
               where (firstHalf, (_:lastHalf)) = splitAt ((((y - 1) * (size bd)) + (x - 1))) bd  

isEmpty :: Int -> Int -> [Char] -> Bool
isEmpty x y bd = ((marker x y bd) == ' ')

isMarked :: Int -> Int -> [Char] -> Bool
isMarked x y bd = not (isEmpty x y bd)

isMarkedBy :: Int -> Int -> [Char] -> Char -> Bool
isMarkedBy x y bd pl = ((marker x y bd) == pl)             

marker :: Int -> Int -> [Char] -> Char
marker x y bd   |  (((y - 1) * (size bd)) + (x - 1)) < 0            = ' '
                |  (((y - 1) * (size bd)) + (x - 1)) >= (length bd) = ' ' 
                |  otherwise                                        = head lastHalf
                where lastHalf = drop ((((y - 1) * (size bd)) + (x - 1))) bd

isFull :: [Char] -> Bool
isFull []        = True
isFull (' ':rem) = False 
isFull (_:rem)   = isFull rem

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

isDraw :: [Char] -> Bool
isDraw bd = (isFull bd) && (not (isWonBy bd 'X')) && (not (isWonBy bd 'O'))

isGameOver :: [Char] -> Bool
isGameOver bd = (isDraw bd) || (isWonBy bd 'X') || (isWonBy bd 'O')

boardToStr :: (Char -> Char) -> [Char] -> [Char]
boardToStr playerToChar bd = allRowsToStrings playerToChar bd (size bd)

allRowsToStrings :: (Char -> Char) -> [Char] -> Int -> [Char]
allRowsToStrings playerToChar bd 0       = []
allRowsToStrings playerToChar bd currRow = (allRowsToStrings playerToChar bd (currRow - 1)) ++ (rowToString playerToChar (row currRow bd)) ++ ['\n'] 

rowToString :: (Char -> Char) -> [Char] -> [Char]
rowToString _            []      = []
rowToString playerToChar (h:rem) = [(playerToChar h)] ++ [' '] ++ (rowToString playerToChar rem)
 
