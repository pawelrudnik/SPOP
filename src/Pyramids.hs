module Pyramids
where
import Data.List

-- utowrzenie typów danych dla wskazówek i tablicy z rozwiązaniem
data Board = Board [[Int]] deriving (Show, Read)
data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving (Show, Read)
type Cell = (Int, Int)

-- sprawdzenie, czy wymiary łamigłówki i podane wartości sa prawidłowe
checkFile :: Piramidy -> Bool
checkFile (Piramidy [][][][]) = False
checkFile (Piramidy top bottom left right) = all (== True) [(bottomL == size), (leftL == size), (rightL == size), all (<= Just size) top, all (<= Just size) bottom, all (<= Just size) left, all (<= Just size) right]
						where size = length top
						      bottomL = length bottom
						      leftL = length left
						      rightL = length right

-- utowrzenie pustej tablicy
createEmptyBoard :: Piramidy -> Board
createEmptyBoard (Piramidy row _ _ _) = Board $ replicate dim $ replicate dim $ 0
						where dim = length row

getSize :: Board -> Int
getSize (Board rows)  = length $ rows !! 0

-- umieszczenie piramidy o danej wysokości we wskazanej komórce tablicy
placePyramidOnBoard :: Board -> Cell -> Int -> Board
placePyramidOnBoard (Board oldRows) (x,y) num = newBoard
	where
	divBoard = splitAt x oldRows
	oldRows1 = fst divBoard
	oldRows2 = tail (snd divBoard)
	row = head (snd divBoard)
	divRow = splitAt y row
	oldCol1 = fst divRow
	oldCol2 = tail (snd divRow)
	newRow = oldCol1 ++ [num] ++ oldCol2
	newBoard = Board (oldRows1 ++ [newRow] ++ oldRows2)

isOnBoard :: Int -> Cell -> Bool
isOnBoard size (x,y) = 
	if ((x < size && x>=0) && (y < size && y>=0)) then True
	else False

getCell :: Board -> Cell -> Int
getCell (Board rows) (x,y) = (rows !! x) !! y

previousCell :: Board -> Cell -> Cell
previousCell board (0,0) = ((getSize board - 1), (getSize board)) 
previousCell board (x,0) = (x-1, (getSize board) -1)
previousCell board (x,y) = (x, y-1)



