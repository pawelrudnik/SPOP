module Solve
where

import Pyramids
import Data.List

-- funkcja rozwiązująca łamigłówkę (pobiera wskazówki i tablicę, na której mają zostac rozmieszczone piramidy)
-- rozpoczyna rozwiązanie próbując umieścić na polu (0,0) piramidę o max wysokości

solvePuzzle :: Piramidy -> Board -> Board
solvePuzzle pyramids board = nextStep pyramids board pyrOfHeight (0,0)
		where
		pyrOfHeight = getSize board
			
-- funkcja wywoływana po umieszczeniu na tablicy piramidy

nextStep :: Piramidy -> Board -> Int -> Cell -> Board
nextStep pyramids board height cell = 
	if (isOnBoard (getSize board) cell) then		--sprawdza czy wypełniono cały wiersz
		pyramidOnCell pyramids board cell height	-- jeśli nie to próbuje wstawić piramidę na wskazanym polu

	else if (isLineOk pyramids board cell) then		-- sprawdza, czy poprawnie wypełniono wiersz
		
		if (isNextRow cell) then					
			nextStep pyramids board height (startNextRow cell)
		
		else board

	else prevStep pyramids board prevHeight prevCell	-- jeśli niepoprawnie wypełniono wiersz to zmienia wartość wstawioną dla ostatniej komórki
	
		where
		isNextRow (x, _) = not(x ==  ((getSize board) - 1))
		startNextRow (x,y) = (x+1, 0)
		prevCell = previousCell board cell
		prevHeight = getCell board prevCell



-- funkcja próbująca umieścić piramidę na polu
-- zaczyna od umieszczenia piramidy o max wysokości, jeśli niemożliwe to w kolejnych krokach zmniejsza wysokość
pyramidOnCell :: Piramidy -> Board -> Cell -> Int -> Board
pyramidOnCell pyramids board (x,y) h = 
	if (h > 0) then

		if ((isUnique board (x,y) h) && (pyramidConstraint pyramids board (x,y) h)) then		--sprawdza czy można umieścić piramidę
			nextStep pyramids (placePyramidOnBoard board (x,y) h) (getSize board) (x, y+1) 
		else 												
			pyramidOnCell pyramids board (x,y) (h-1)

	else prevStep pyramids (placePyramidOnBoard board (x,y) 0) prevHeight prevCell		-- jeśli żadna wysokość nie pasuje na danej komórce, to umieszcza 0 i zmniejsza wartość poprzedniej komórki
	
		where 
		prevCell = previousCell board (x,y)
		prevHeight = getCell board prevCell


-- funkcja wywoływana, gdy trzeba było cofnąć się do poprzedniej komórki
prevStep :: Piramidy -> Board -> Int -> Cell -> Board
prevStep pyramids board height cell = 
	if (height == 1) then 								-- jeśli była 1 to umieszcza 0 i probuje umiescic w poprzedniej komorce wartość o 1 mniejszą
		nextStep pyramids (placePyramidOnBoard board cell 0) (prevHeight-1) prevCell
	else nextStep pyramids board (height-1) cell					-- jeśli była >1 to próbuje umieścić na tym polu wartość o 1 mniejszą
		where 
		prevCell = previousCell board cell
		prevHeight = getCell board prevCell


-- funkcja sprawdzająca, czy dana wartość występuje już w wierszu lub kolumnie, na skrzyżowaniu których leży komórka
isUnique :: Board -> Cell -> Int -> Bool
isUnique (Board rows) (x,y) h = 
	if (inRow || inCol) then False
	else True
		where 
		inRow = elem h (rows !! x)
		inCol = elem h $(transpose rows) !! y


-- funkcja sprawdzająca, czy piramida o danej wysokości nie jest za wysoka by stanąć w danym miejscu
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


-- funkcja sprawdzająca, czy uzupelniony cały wiersz jest zgodny ze wszystkimi ograniczeniami narzuconymi przez wskazówki
isLineOk :: Piramidy -> Board -> Cell -> Bool
isLineOk (Piramidy t b l r) (Board rows) (x,y) = fromLeft  && fromRight && fromTop && fromBottom
	where
	fromLeft = isRowOk (l !! x) (countVisible 1 (rows !! x))
	fromRight = isRowOk (r !! x) (countVisible 1 (reverse (rows !! x)))
	fromTop = isColOk t (transpose rows) False
	fromBottom = isColOk b (map reverse (transpose rows)) True


-- sprawdzenie, czy w danym wierszu widać tyle piramid ile narzucają ograniczenia
isRowOk :: Maybe Int -> Int -> Bool
isRowOk Nothing _ = True
isRowOk (Just constraint) actualNum = constraint == actualNum


-- sprawdzenie, czy w danej kolumnie możliwe jest spełnienie ograniczeń
isColOk :: [Maybe Int] -> [[Int]] -> Bool -> Bool
isColOk constraints cols up =  and $ zipWith (checkCol) constraints actualNums
	where
	actualNums = map (countVisible 1) cols
	checkCol :: Maybe Int -> Int -> Bool
	checkCol Nothing _ = True
	checkCol (Just constraint) actualNum = 
		if (up) then constraint <= actualNum else constraint >= actualNum  -- jeśli liczymy od dołu to sprawdzamy, czy po uzupełnieniu pustych wartości rosnąco, nie widzimy za mało piramid
										   -- jeśli od góry to czy nie widzimy więcej piramid niż można w danej kolumnie


 -- obliczenie ile piramid jest widocznych w danym wierszu / kolumnie
countVisible :: Int -> [Int] -> Int
countVisible i [x] = i
countVisible i (x:xs) 	
	|x == 0		= countVisible i (fillAsc (xs) (length (0:xs))) -- jeśli na początku sprawdzanej listy są zera, to sa uzupełniane
	|x < head xs 	= countVisible (i+1) xs
	|head xs == 0	= i+1						-- jeśli na końcu listy są zera, to zwraca wartość piramid widocznych do tego momentu
	|otherwise	= countVisible i ([x] ++ tail xs)


-- usupełnia zera na danej liście rosnąco, wartościami, które jeszcze w nim nie wystąpiły
fillAsc [x] n		= sort([1..n] \\ [x]) ++ [x]
fillAsc (x:xs) n
	|x == 0		= fillAsc xs n
	|otherwise	= sort([1..n] \\ (x:xs)) ++ (x:xs)  

