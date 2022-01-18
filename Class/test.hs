fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)
-- fac n = if n == 0 then 1 else n * fac(n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib(n-1)) + (fib(n-2))

len ::  [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

squaresTo :: Int -> [Int]
squaresTo n = [k^2 | k <- [0..n]]

deadband :: Float -> Float
deadband n = if (abs(n)) < 0.2 then 0 else n  


main :: IO()
main = do
    print $ fac 5

    print $ deadband 0.8
    print $ deadband(-0.65)
    print $ deadband 0.1

    print $ fib 3

    print $ len [1,2,3]
    print $ len [1..10]

    print $ squaresTo 10
