{--
  CSCI 312 Homework #1

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw01.html
--}

module Hw01 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

type Node = String
type DAG = Map.Map Node (Set.Set Node)

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"

g = Map.fromList [(a, Set.fromList [b,c]),
                  (b, Set.fromList [d]),
                  (c, Set.fromList [d]),
                  (d, Set.fromList []),
                  (e, Set.fromList [a, c])]

-- Put your functions here --------------------
-- https://www.quora.com/What-does-x-xs-mean-in-Haskell
sumUp :: [Int] -> Int
-- if the list if empty, just return 0
sumUp []     = 0
-- if it's not empty, add the head and the sum of the rest of the list
sumUp (x:xs) = x + sumUp xs


evens :: [Int] -> [Int]
-- establish a list to be returned
returnedList1 = []
-- if the list is empty, just return the list
evens [] = returnedList1
-- if it's not empty, if the number is even, append it and run evens on the rest of the list
evens (x:xs) =  if even x then returnedList1 ++ [x] ++ evens xs
                -- if the number is odd, then skip over it and check the rest of the list
                else evens xs



incAll :: [Int] -> [Int]
returnedList2 = []
incAll [] = returnedList2
incAll (x:xs) = returnedList2 ++ [x + 1] ++ incAll xs


incBy :: Int -> [Int] -> [Int]
returnedList3 = []
-- if we've hit the end of the list, just return it
incBy num [] = returnedList3
incBy num (x:xs) = returnedList3 ++ [x + num] ++ incBy num xs


append :: [Int] -> [Int] -> [Int]
append (x:xs) [] = (x:xs)
append [] (y:ys) = (y:ys)


data IntTree = Empty | Node IntTree Int IntTree deriving (Eq, Show)

isLeaf :: IntTree -> Bool
isLeaf Empty = True
isLeaf (Node l x r) = False 
-- Tests ----------------------------------------

main = do

    putStrLn "Problem 1: natural recursion -----------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll [1,2,3,4,5,6,7,8,9]

    putStr "Should be [3,4,5,6,7,8,9,10,11]: "
    print $ incBy 2 [1,2,3,4,5,6,7,8,9]

    putStr "Should be 1,2,3]: "
    print $ append [] [1,2,3]

    putStr "Should be [1,2,3]: "
    print $ append [1,2,3] []

--    putStr "Should be [1,2,3,4,5,6]: "
--    print $ append [1,2,3] [4,5,6]

    putStrLn "\nProblem 2: data types -----------------------------------------\n"

--    putStr "Should be True: "
--    print $ isLeaf Empty

--    putStr "Should be True: "
--    print $ isLeaf (Node Empty 3 Empty)

--    putStr "Should be False: "
--    print $ isLeaf (Node (Node Empty 1 Empty) 2 Empty)

--    putStr "Should be 10: "
--    print $ sumTree (Node (Node Empty 1 Empty) 3 (Node Empty 2 (Node Empty 4 Empty)))

--    putStr "Should be [2,7]: "
--    print $ fringe (Node (Node Empty 1 (Node Empty 2 Empty))
--                          5
--                          (Node (Node Empty 7 Empty) 10 Empty))

    putStrLn "\nProblem 3: binary search trees --------------------------------\n"

--    putStr "Should be True: "
--    print $ isBST (Node (Node Empty 2 Empty)  4 (Node Empty 5 Empty))

--    putStr "Should be False: "
--    print $ isBST (Node (Node Empty 5 Empty)  4 (Node Empty 2 Empty))

--    putStrLn "\nProblem 4: map and filter -------------------------------------\n"

--    putStr "Should be 6: "
--    print $ sumUp' [1,2,3]

--    putStr "Should be [2,4,6,8]: "
--    print $ evens' [1,2,3,4,5,6,7,8,9]

--    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
--    print $ incAll' [1,2,3,4,5,6,7,8,9]

--    putStr "Should be 3,4,5,6,7,8,9,10,11]: "
--    print $ incBy' 2 [1,2,3,4,5,6,7,8,9]

--    putStrLn "\nProblem 5: defining higher-order functions --------------------\n"

--    putStr "Should be [1,4,16,25]: "
--    print $ map1 (\x -> x * x) [1,2,3,4,5]

--    putStr "Should be [1,3,5,7,9]: "
--    print $ filter1 odd [0,1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 6: Maybe and Either ------------------------------------\n"

--    putStr "Should be [0.0,1.0,2.0,3.0]: "
--    print $ mapMaybe sqrt' [0,-1,1,-4,4,9,-9]

    putStrLn "\nProblem 7: Creating polymorphic data types ---------------------\n"

--    putStr "Should be (\"hello\", 3): "
--    print $ swap (3, "hello") 

--    putStr "Should be [(0,1),(2,3),(4,5),(6,7),(8,9)]: "
--    print $ pairUp [0,2,4,6,8] [1,3,5,7,9]

--    putStr "Should be ([0,2,4,6,8],[1,3,5,7,9]): "
--    print $ splitUp [(0,1),(2,3),(4,5),(6,7),(8,9)]

--    putStr "Should be (15, 5): "
--    print $ sumAndLength [1,2,3,4,5]

--    case div' 1 0 of
--      Right val -> print $ val
--      Left  msg -> putStrLn msg

--    case div' 1 2 of
--      Right val -> print $ val
--      Left  msg -> putStrLn msg

    putStrLn "\nProblem 8: maps and sets --------------------------------------\n"

--    putStr "Should be True: "
--    print $ hasPath g a d

--    putStr "Should be False: "
--    print $ hasPath g a e
