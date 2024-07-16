module Main (main) where
import Board
import Test.HUnit
import qualified System.Exit as Exit

test_board :: Board
t_width :: Int
t_height :: Int
t_width = 100
t_height = 100

test_board = Board
  {
    cells =[(0,0), (t_width-1, t_height-1), (t_width-1,0), (0,t_height-1)],
    width = t_width, height = t_height}

point :: Position
point = (5,5)

rnpoint :: Position
rnpoint = (0,0)

edge_point0 :: Position
edge_point0 = (0,0)
edge_point1 :: Position
edge_point1 = (t_width-1,0)
edge_point2 :: Position
edge_point2 = (0,t_height-1)
edge_point3 :: Position
edge_point3 = (t_width-1,t_height-1)

wrap_neighs0 :: [Position]
wrap_neighs0 = neighood test_board edge_point0
wrap_neighs1 :: [Position]
wrap_neighs1 = neighood test_board edge_point1
wrap_neighs2 :: [Position]
wrap_neighs2 = neighood test_board edge_point2
wrap_neighs3 :: [Position]
wrap_neighs3 = neighood test_board edge_point3


wrapped_list0 :: [Position]
wrapped_list0 = [(99,99),(0,99),(1,99)
               ,(99,0),(1,0)
               ,(99,1),(0,1),(1,1)]

wrapped_list1 :: [Position]
wrapped_list1 = [(98,99),(99,99),(0,99)
               ,(98,0),(0,0)
               ,(98,1),(99,1),(0,1)]

wrapped_list2 :: [Position]
wrapped_list2 = [(99,98),(0,98),(1,98)
               ,(99,99),(1,99)
               ,(99,0),(0,0),(1,0)]

wrapped_list3 :: [Position]
wrapped_list3 = [(98,98),(99,98),(0,98)
               ,(98,99),(0,99)
               ,(98,0),(99,0),(0,0)]

corners :: [Position]
corners = [edge_point0, edge_point1, edge_point2, edge_point3]
-- x :: Int
-- x = fst point
-- y :: Int
-- y = snd point

test_neighc0 :: Test
test_neighc0 = TestCase $ assertBool "Should be element" $ elem (4,4) (neighc point)

test_neighc1 :: Test
test_neighc1 = TestCase $ assertBool "Should be element" $ elem (5,4) (neighc point)

test_neighc2 :: Test
test_neighc2 = TestCase $ assertBool "Should be element" $ elem (6,4) (neighc point)


test_neighc3 :: Test
test_neighc3 = TestCase $ assertBool "Should be element" $ elem (4,5) (neighc point)

test_neighc4 :: Test
test_neighc4 = TestCase $ assertBool "Should be element" $ elem (6,5) (neighc point)


test_neighc5 :: Test
test_neighc5 = TestCase $ assertBool "Should be element" $ elem (4,6) (neighc point)

test_neighc6 :: Test
test_neighc6 = TestCase $ assertBool "Should be element" $ elem (5,6) (neighc point)

test_neighc7 :: Test
test_neighc7 = TestCase $ assertBool "Should be element" $ elem (6,6) (neighc point)

test_neighc :: Test
test_neighc = TestCase (assertBool "Should return list"
                         (and [b | b <- fmap (\e -> elem e ns) vs]))
                where
                  ns = neighc point
                  x = fst point
                  y = snd point
                  vs = [(x-1,y-1),(x,y-1),(x+1,y-1),
                        (x-1,y),(x+1,y),
                        (x-1,y+1),(x,y+1),(x+1,y+1)]

test_raw_neigh :: Test
test_raw_neigh = TestList [TestLabel "Neighborhood test: " test_neighc,
                          TestLabel "Point test: " test_neighc0,
                          TestLabel "Point test: " test_neighc1,
                          TestLabel "Point test: " test_neighc2,
                          TestLabel "Point test: " test_neighc3,
                          TestLabel "Point test: " test_neighc4,
                          TestLabel "Point test: " test_neighc5,
                          TestLabel "Point test: " test_neighc6,
                          TestLabel "Point test: " test_neighc7]

test_wrapl0 :: Test
test_wrapl0 = TestCase (assertEqual "Wrapped" wrapped_list0 wrap_neighs0)

test_wrapl1 :: Test
test_wrapl1 = TestCase (assertEqual "Wrapped" wrapped_list1 wrap_neighs1)

test_wrapl2 :: Test
test_wrapl2 = TestCase (assertEqual "Wrapped" wrapped_list2 wrap_neighs2)

test_wrapl3 :: Test
test_wrapl3 = TestCase (assertEqual "Wrapped" wrapped_list3 wrap_neighs3)

test_wrapl :: Test
test_wrapl = TestList [TestLabel "Superior left corner" test_wrapl0
                     ,TestLabel "Superior right corner" test_wrapl1
                     ,TestLabel "Inferior left corner" test_wrapl2
                     ,TestLabel "Inferior right corner" test_wrapl3]

test_is_alive :: Test
test_is_alive = TestCase $
  assertBool "Should be alive" $
  and $ fmap (isAlive test_board) corners

test_is_dead :: Test
test_is_dead = TestCase $
  assertBool "Should be deads" $
  not $ and $ fmap (isAlive test_board) [(1,0), (1,99), (99,98)]

test_survivors :: Test
test_survivors = TestCase $
  assertBool "Stable cells" $
  and $ [b | b <- fmap (\s -> elem s (survivors test_board)) corners]


tests :: Test
tests = TestList [TestLabel "First Tests" test_raw_neigh
                 ,TestLabel "Wrap neighborhood" test_wrapl
                 ,TestLabel "Alives" test_is_alive
                 ,TestLabel "Deads" test_is_dead
                 ,TestLabel "Survivors" test_survivors]


main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
