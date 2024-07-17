module IO where
import Text.Read

clean_screen :: IO ()
clean_screen = putStr "\ESC[2J"

get_board_data :: String -> IO Int
get_board_data m = do
  putStrLn m
  line  <- getLine
  let inf = (readMaybe line :: Maybe Int)
  case inf of
    Just x -> return x
    _      -> get_board_data m


string_to_tuple :: String -> Maybe (Int, Int)
string_to_tuple s = return (read s :: (Int, Int))

get_positions :: IO [(Int, Int)]
get_positions = do
  putStrLn "Type validad coordinates, finish with empty line:"
  line <- getLine
  case line of
    "" -> return []
    _ ->
      let headp = string_to_tuple line in
        case headp of
          Just t -> do
            xs <- get_positions
            return (t:xs)
          Nothing -> get_positions

draw_at :: (Int, Int) -> String -> IO ()
draw_at p s = do
    goto p
    putStr s

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
