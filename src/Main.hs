import System.Environment

main = do
  args <- getArgs
  if length args < 1
    then error "usage: Pyramids <filename>"
  else do
    let filename = last args
    content <- readFile filename
    putStrLn content
