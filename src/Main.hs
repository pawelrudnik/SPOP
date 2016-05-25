import Pyramids

main = do
  
  putStrLn "Wpisz nazwę pliku, z którego ma być wczytana łamigłówka:"
  filename <- getLine
  content <- readFile filename
  
  let input = read content::Piramidy
  let output = createEmptyBoard input

  putStrLn $ show output

  putStrLn "Wciśnij klawisz enter aby zakończyć"
  key <- getChar
  return key