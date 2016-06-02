import Pyramids
import Solve

{- główna funkcja: pobiera nazwę pliku z łamigłówką i wczytuje jego zawartość (wskazówki)
   wypełnia tablicę zerami i rozwiązuje łamigłówkę
   jeśli znajdzie rozwiązanie to je wyświetla -}

main = do
  
  putStrLn "Wpisz nazwę pliku, z którego ma być wczytana łamigłówka:"
  filename <- getLine
  content <- readFile filename
  
  let input = read content::Piramidy
  if (checkFile input) then do
			 	putStrLn "Wczytano poprawną łamigłówkę"
			else
				putStrLn "Wczytana łamigłówka jest niepoprawna"

  let board = createEmptyBoard input

  let solution = solvePuzzle input board
  putStrLn $ show solution

  putStrLn "Wciśnij klawisz enter aby zakończyć"
  key <- getChar
  return key
