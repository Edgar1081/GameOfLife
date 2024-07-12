module Main where

import qualified IO
import qualified Board

showcells :: Board.Board -> IO ()
showcells b = sequence_ [IO.draw_at p "X" | p <- cells b ]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  IO.clean_screen
  w <- IO.get_board_data "Type a valid width : "
  h <- IO.get_board_data "Type a valid height: "
  positions <- IO.get_positions
  putStrLn (show positions)

  --let b = Board.Board positions w h in
