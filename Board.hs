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


mark = False
isEmpty = False
isMarked = False
isMarkedBy = False
marker = False
isFull = False
isWonBy = False
isDraw = False
isGameOver = False
boardToStr = False
