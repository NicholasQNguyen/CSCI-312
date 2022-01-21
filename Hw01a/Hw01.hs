{--
:60
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

-- "Problem 1: natural recursion --------------
sumUp :: [Int] -> Int
-- if the list if empty, just return 0
sumUp [] = 0
-- if it's not empty, add the head and the sum of the rest of the list
sumUp (x:xs) = x + sumUp xs


evens :: [Int] -> [Int]
-- if the list is empty, just return the list
evens [] = []
-- if it's not empty, if the number is even, append it and run evens on the rest of the list
evens (x:xs) = if even x then [x] ++ evens xs
               -- if the number is odd, then skip over it and check the rest of the list
               else evens xs

incAll :: [Int] -> [Int]
incAll [] = []
incAll(x:xs) = [x + 1] ++ incAll xs

incBy :: Int -> [Int] -> [Int]
-- if we've hit the end of the list, just return it
incBy num [] = []
incBy num (x:xs) = [x + num] ++ incBy num xs


append :: [Int] -> [Int] -> [Int]
-- account for if one of the lists is empty
append (x:xs) [] = (x:xs)
append [] (y:ys) = (y:ys)
append (x:xs) l2 = x : (append xs l2)


-- "Problem 2: data types --------------

data IntTree = Empty | Node IntTree Int IntTree deriving (Eq, Show)

isLeaf :: IntTree -> Bool
-- If the tree is empty, we know it's a leaf
isLeaf Empty = True
-- If botht the left and right nodes are empty, we know it's a leaf
isLeaf (Node l x r) = if l == Empty && r == Empty
                      then True
                      -- else we know it's not a leaf
                      else False 


sumTree :: IntTree -> Int
-- If the tree is empty, return the number
sumTree Empty = 0
-- add the node's value + the values of the left and right nodes' values
sumTree (Node l x r) = x + sumTree l + sumTree r


fringe :: IntTree -> [Int]
fringe Empty = []
fringe (Node Empty value Empty) = [value]
fringe (Node left value right) = (fringe left) ++ (fringe right)


-- "Problem 3:binary search trees"-----

-- Helper function used in isBST to get the Int value out of a node 
getValue :: IntTree -> Int
getValue (Node l x r) = x


isBST :: IntTree -> Bool
isBST Empty = True
-- Check the left node's value, if it's less then the current one, keep checking
isBST (Node Empty x Empty) = True
isBST (Node left n Empty) = isBST left && (n> getValue left)
isBST (Node Empty n right) = isBST right && (n < getValue right)
isBST (Node left n right) = isBST left && (n>(getValue left)) && isBST right && (n<(getValue right))


-- "Problem 4: map and filter" ---------

-- https://stackoverflow.com/questions/1757740/how-does-foldr-work
sumUp' :: [Int] -> Int
sumUp' l = foldr(+) 0 l

evens' :: [Int] -> [Int]
evens' l = filter (even) l 


incInt :: Int -> Int -> Int
incInt n k = n + k


-- http://zvon.org/other/haskell/Outputprelude/map_f.html
incAll' :: [Int] -> [Int]
incAll' l = map (incInt 1) l


incBy' :: Int -> [Int] -> [Int]
incBy' n l = map (incInt n) l 


-- "Problem 5: defining higher-order functions"

map1 :: (a -> b) -> [a] -> [b]
map1 function [] = []
map1 function (x:xs) = [function(x)] ++ map1 (function) (xs)


-- TODO finish
filterl :: (a -> Bool) -> [a] -> [a]
filterl = ["ASDFGHJ"]



-- "Problem 6: Maybe and Either" -------

-- TODO finish
{--
data Maybe Float = Just Float | Nothing


sqrt' :: Float -> Maybe Float
sqrt' x = if x<0
          then Nothing
          else sqrt(x)


data Either a b = Left a | Right b


div' :: Float -> Float -> Hw01.Either String Float
div' x y = if y == 0
           then "PROBLEM"
           else x/y
--}

-- "Problem 7: Creating polymorphic datatypes"

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- TODO finish

{--
pairUp :: [a] -> [b] -> [(a,b)]
pairUp (x:xs) (y:ys) = [(x,y)] ++ (pairUp(xs ys))
--}

splitUp :: [(a, b)] -> ([a], [b])
splitUp [] = ([], [])
splitUp ((x1, x2):xs) = let split = (splitUp xs) 
                        in (x1:(fst split), x2:(snd split))

-- TODO finish
sumAndLength :: [Int] -> (Int,Int)
sumAndLength l = ((foldr(+) 0 l), length l)


-- "Problem 8: maps and sets" ----------


neighbors :: DAG -> Node -> Set.Set Node
neighbors dag node = (Map.!) dag node -- similar to Python dag[node]



-- any' :: Set.Set Bool -> Bool


hasPath :: DAG -> Node -> Node -> Bool
hasPath dag node1 node2 = let nbrs = ((Map.!) dag node1)
                          in elem node2 nbrs || any (\n -> hasPath dag n node2) nbrs

 
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

    putStr "Should be [1,2,3,4,5,6]: "
    print $ append [1,2,3] [4,5,6]

    putStrLn "\nProblem 2: data types -----------------------------------------\n"

    putStr "Should be True: "
    print $ isLeaf Empty

    putStr "Should be True: "
    print $ isLeaf (Node Empty 3 Empty)

    putStr "Should be False: "
    print $ isLeaf (Node (Node Empty 1 Empty) 2 Empty)

    -- I added this test
    putStr "Should be 3: "
    print $ sumTree (Node Empty 3 Empty)

    putStr "Should be 10: "
    print $ sumTree (Node (Node Empty 1 Empty) 3 (Node Empty 2 (Node Empty 4 Empty)))

    putStr "Should be [2,7]: "
    print $ fringe (Node (Node Empty 1 (Node Empty 2 Empty))
                          5
                          (Node (Node Empty 7 Empty) 10 Empty))

    putStrLn "\nProblem 3: binary search trees --------------------------------\n"
    
    -- I added this test
    putStr"Should be 4: "
    print $ getValue (Node Empty 4 Empty)

    putStr "Should be True: "
    print $ isBST (Node (Node Empty 2 Empty)  4 (Node Empty 5 Empty))

    putStr "Should be False: "
    print $ isBST (Node (Node Empty 5 Empty)  4 (Node Empty 2 Empty))

    putStrLn "\nProblem 4: map and filter -------------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp' [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll' [1,2,3,4,5,6,7,8,9]

    putStr "Should be 3,4,5,6,7,8,9,10,11]: "
    print $ incBy' 2 [1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 5: defining higher-order functions --------------------\n"

    putStr "Should be [1,4,16,25]: "
    print $ map1 (\x -> x * x) [1,2,3,4,5]

--    putStr "Should be [1,3,5,7,9]: "
--    print $ filter1 odd [0,1,2,3,4,5,6,7,8,9]

    putStrLn "\nProblem 6: Maybe and Either ------------------------------------\n"

--    putStr "Should be [0.0,1.0,2.0,3.0]: "
--    print $ mapMaybe sqrt' [0,-1,1,-4,4,9,-9]

    putStrLn "\nProblem 7: Creating polymorphic data types ---------------------\n"

    putStr "Should be (\"hello\", 3): "
    print $ swap (3, "hello") 

--    putStr "Should be [(0,1),(2,3),(4,5),(6,7),(8,9)]: "
--    print $ pairUp [0,2,4,6,8] [1,3,5,7,9]

    putStr "Should be ([0,2,4,6,8],[1,3,5,7,9]): "
    print $ splitUp [(0,1),(2,3),(4,5),(6,7),(8,9)]

    putStr "Should be (15, 5): "
    print $ sumAndLength [1,2,3,4,5]

--    case div' 1 0 of
--      Right val -> print $ val
--      Left  msg -> putStrLn msg

--    case div' 1 2 of
--      Right val -> print $ val
--      Left  msg -> putStrLn msg

    putStrLn "\nProblem 8: maps and sets --------------------------------------\n"

    print $ neighbors g a

    putStr "Should be True: "
    print $ hasPath g a d

    putStr "Should be False: "
    print $ hasPath g a e

