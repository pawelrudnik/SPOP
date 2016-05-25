import System.Environment

main = do
  putStrLn "Wpisz nazwę pliku, z którego ma być wczytana łamigłówka:"
  filename <- getLine
  content <- readFile filename
  putStrLn content
