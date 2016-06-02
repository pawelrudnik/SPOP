module Solve
where

import Pyramids
import Data.List

solvePuzzle :: Piramidy -> Board -> Board
solvePuzzle pyramids board = nextStep pyramids board pyrOfHeight (0,0)
		where
		pyrOfHeight = getSize board
			
 
nextStep :: Piramidy -> Board -> Int -> Cell -> Board
nextStep pyramids board height cell = 
	if (isOnBoard (getSize board) cell) then 
		pyramidOnCell pyramids board cell height

	else if (isLineOk pyramids board cell) then
		
		if (isNextRow cell) then
			nextStep pyramids board height (startNextRow cell)
		
		else board

	else prevStep pyramids board prevHeight prevCell
	
		where
		isNextRow (x, _) = not(x ==  ((getSize board) - 1))
		startNextRow (x,y) = (x+1, 0)
		prevCell = previousCell board cell
		prevHeight = getCell board prevCell


pyramidOnCell :: Piramidy -> Board -> Cell -> Int -> Board
pyramidOnCell pyramids board (x,y) h = 
	if (h > 0) then

		if ((isUnique board (x,y) h) && (pyramidConstraint pyramids board (x,y) h)) then
			nextStep pyramids (placePyramidOnBoard board (x,y) h) (getSize board) (x, y+1) 
		else 
			pyramidOnCell pyramids board (x,y) (h-1)

	else prevStep pyramids (placePyramidOnBoard board (x,y) 0) prevHeight prevCell
	
		where 
		prevCell = previousCell board (x,y)
		prevHeight = getCell board prevCell


prevStep :: Piramidy -> Board -> Int -> Cell -> Board
prevStep pyramids board height cell = 
	if (height == 1) then 
		nextStep pyramids (placePyramidOnBoard board cell 0) (prevHeight-1) prevCell
	else nextStep pyramids board (height-1) cell
		where 
		prevCell = previousCell board cell
		prevHeight = getCell board prevCell


isUnique :: Board -> Cell -> Int -> Bool
isUnique (Board rows) (x,y) h = 
	if (inRow || inCol) then False
	else True
		where 
		inRow = elem h (rows !! x)
		inCol = elem h $(transpose rows) !! y


pyramidConstraint :: Piramidy -> Board -> Cell -> Int -> Bool
pyramidConstraint (Piramidy t b l r) board (x,y) h = (checkLeft && checkRight && checkTop && checkBottom)
	where 
	size = getSize board
	checkLeft = canBeVisible (l !! x) h y size
	checkRight = canBeVisible (r !! x) h (size - 1 - y) size
	checkTop = canBeVisible (t !! y) h x size
	checkBottom = canBeVisible (b !! y) h (size - 1 - x) size
	

canBeVisible :: Maybe Int -> Int -> Int -> Int -> Bool 
canBeVisible Nothing _ _ _ = True
canBeVisible (Just howMany) h place max = howMany <= (max - h + place + 1)


isLineOk :: Piramidy -> Board -> Cell -> Bool
isLineOk (Piramidy t b l r) (Board rows) (x,y) = fromLeft  && fromRight && fromTop && fromBottom
	where
	fromLeft = isRowOk (l !! x) (countVisible 1 (rows !! x))
	fromRight = isRowOk (r !! x) (countVisible 1 (reverse (rows !! x)))
	fromTop = isColOk t (transpose rows)
	fromBottom = isColOk b (map reverse (transpose rows))


isRowOk :: Maybe Int -> Int -> Bool
isRowOk Nothing _ = True
isRowOk (Just constraint) actualNum = constraint == actualNum


isColOk :: [Maybe Int] -> [[Int]] -> Bool
isColOk constraints cols =  and $ zipWith (checkCol) constraints actualNums
	where
	actualNums = map (countVisible 1) cols
	checkCol :: Maybe Int -> Int -> Bool
	checkCol Nothing _ = True
	checkCol (Just constraint) actualNum = constraint >= actualNum


countVisible :: Int -> [Int] -> Int
countVisible i [x] = i
--countVisible i (0:xs) = countVisible i (fillAsc (xs) (length (0:xs)))
countVisible i (x:xs) 	
	|x < head xs 	= countVisible (i+1) xs
--	|head xs == 0	= i + length xs
	|otherwise	= countVisible i ([x] ++ tail xs)

{-
fillAsc (x:xs) n
	|x == 0		= fillAsc xs n
	|otherwise	= sort((x:xs) \\ [1..n]) ++ (x:xs)  
-}


