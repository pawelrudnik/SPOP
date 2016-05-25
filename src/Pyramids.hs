module Pyramids
where
import Data.List

data Board = Board [[Int]] deriving (Show, Read)
data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving (Show, Read)

getSize :: Piramidy -> Int
getSize (Piramidy row _ _ _)  = length row

createEmptyBoard :: Piramidy -> Board
createEmptyBoard input = Board $ replicate dim $ replicate dim $ 0
						where dim = getSize input