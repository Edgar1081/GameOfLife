module Board where

type Position = (Int, Int)

data Board = Board
  {
    cells :: [Position]
  ,width :: Int
  ,height :: Int
  }

{-Returns raw neighborhood values given a Position-}
neighc :: Position -> [Position]
neighc (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),
               (x-1,y), (x+1, y),
               (x-1,y+1),(x,y+1),(x+1,y+1)]

{-Decide if a Position is validate to birth-}
birth :: Board -> Position -> Bool
birth b p = (isDead b p) && ((livingneigh b p) == 3)

{-Remove duplicates values in lists-}
remdups :: Eq a => [a] -> [a]
remdups [] = []
remdups (x:xs) = x : remdups (filter (/= x) xs)

class Game a where
  next_gen :: a -> a
  isAlive :: a -> Position -> Bool
  neighood :: a -> Position -> [Position]
  livingneigh :: a -> Position -> Int
  livingneigh a = length . neighood a
  survivors :: a -> [Position]
  births :: a -> [Position]
  isDead :: a -> Position -> Bool
  isDead a = not . isAlive a


instance Game Board where
  isAlive b p = (elem p) $ cells b

  neighood b p = fmap
    (\t -> ( fst t `mod` width b, snd t `mod` height b)) $
    neighc p

  survivors b = [ p | p <- cells b,
                  let l = (length $ filter (isAlive b) $ neighood b p) in
                    l == 2 || l == 3]

  births b = filter (birth b)
    (remdups (foldl (++) [ls | cell <- cells b ,ls <- neighood b cell] []))


  next_gen b = b {cells = survivors b ++ births b}
