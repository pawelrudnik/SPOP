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
	else if (isNextRow cell) then
		nextStep pyramids board height (startNextRow cell)
	else board
		where
		isNextRow (x, _) = not(x == getSize board)
		startNextRow (x,y) = (x+1, 0)


pyramidOnCell :: Piramidy -> Board -> Cell -> Int -> Board
pyramidOnCell pyramids board (x,y) h = 
	if (h > 0) then
	--	if ((isUnique board (x,y) h) && (pyramidConstraint pyramids board (x,y) h)) then
	if (isUnique board (x,y) h) then
			nextStep pyramids (placePyramidOnBoard board (x,y) h) (h-1) (x, y+1) 
		else 
		pyramidOnCell pyramids board (x,y) (h-1)
	else pyramidOnCell pyramids board (x,y) (getSize board)


isUnique :: Board -> Cell -> Int -> Bool
isUnique (Board rows) (x,y) h = 
	if (inRow || inCol) then False
	else True
		where 
		inRow = elem h (rows !! x)
		inCol = elem h $(transpose rows) !! y


-- To się wiesza, nie wiem czemu :(

--pyramidConstraint :: Piramidy -> Board -> Cell -> Int -> Bool
--pyramidConstraint (Piramidy t b l r) board (x,y) h = checkLeft
--	where 
--	checkLeft = isVisible (l !! x) h y (getSize board)
	
 
--isVisible Nothing _ _ _ = True
--isVisible (Just howMany) h place max = howMany > (max - h + place + 1)
