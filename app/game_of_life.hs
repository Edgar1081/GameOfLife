module Main where

import qualified IO
import qualified Board

showcells :: Board.Board -> IO ()
showcells b = sequence_ [IO.draw_at p "0" | p <- Board.cells b ]

main :: IO ()
main = do
  IO.clean_screen
  w <- IO.get_board_data "Type a valid width : "
  h <- IO.get_board_data "Type a valid height: "
  positions <- IO.get_positions
  let b = Board.Board positions w h
  IO.clean_screen
  life b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: Board.Board -> IO ()
life b = do
  IO.clean_screen
  showcells b
  wait 1000000
  life (Board.next_gen b)
