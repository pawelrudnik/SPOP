import Pyramids
import Solve

-- główna funkcja: pobiera nazwę pliku z łamigłówką, wczytuje jego zawartość, rozwiązuje i wyświetla rozwiązanie

main = do
  
  putStrLn "Wpisz nazwę pliku, z którego ma być wczytana łamigłówka:"
  filename <- getLine
  content <- readFile filename
  
  let input = read content::Piramidy
  if (checkFile input) then do
			 	putStrLn "Wczytano poprawną łamigłówkę"
			else
				putStrLn "Wczytana łamigłówka jest niepoprawna"

  let output = createEmptyBoard input

  let s = solvePuzzle input output
  putStrLn $ show s

  putStrLn "Wciśnij klawisz enter aby zakończyć"
  key <- getChar
  return key