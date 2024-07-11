module Board where

type Position = (Int, Int)

data Board = Board
  {
    cells :: [Position]
  ,width :: Int
  ,height :: Int
  }

class Game a where
  isAlive :: a -> Position -> Bool
  nex_gen :: a -> a
