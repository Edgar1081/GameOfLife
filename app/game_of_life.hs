module Main where

import qualified Functions

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Functions.clean_screen
  w <- Functions.get_board_data "Type a valid width : "
  h <- Functions.get_board_data "Type a valid height: "
  positions <- Functions.get_positions
  putStrLn (show positions)
  putStr ""
