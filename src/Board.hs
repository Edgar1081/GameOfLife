module Board where

type Position = (Int, Int)

data Board = Board
  {
    cells :: [Position]
  ,width :: Int
  ,height :: Int
  }

neighc :: Position -> [Position]
neighc p =
    let x = fst p in
        let y = snd p in
            [ (xv, y-1) | xv <- [x-1, x, x+1] ] ++
            [ (xv, y) | xv <- [x-1, x+1] ] ++
            [ (xv, y+1) | xv <- [x-1, x, x+1]]

class Game a where
  isAlive :: a -> Position -> Bool
  next_gen :: a -> a
  neighood :: a -> Position -> [Position]
  survivors :: a -> [Position]
  births :: a -> [Position]
  isDead :: a -> Position -> Bool
  isDead a = not . isAlive a

instance Game Board where

  isAlive b p = (elem p) $ cells b

  neighood b p = fmap
    (\t -> ((fst t) - 1 `mod` height b +1, (snd t) -1 `mod` width b)) $
    neighc p

  survivors b = [ p | p <- cells b, let l = (length $ neighood b p) in l == 2 || l == 3]

  next_gen b = b {cells = survivors b ++
                  [(x,y) | x <- [0.. (width b) -1]
                    ,y <- [0 .. (height b) -1]
                    , isAlive b (x,y)]}
