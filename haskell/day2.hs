module Main where

    -- Write a sort that takes a list and returs a sorted list

    -- Let's do a merge sort
    splitHalf :: [a] -> ([a], [a])
    splitHalf list = splitAt (div (length list) 2) list

    merge :: Ord a => [a] -> [a] -> [a]
    merge x [] = x
    merge [] x = x
    merge (x:xs) (y:ys)
        | x <= y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

    sort :: Ord a => [a] -> [a]
    sort [] = []
    sort [x] = [x]
    sort list = merge (sort firstHalf) (sort secondHalf)
        where (firstHalf, secondHalf) = splitHalf list

    -- Write a sort that takes a list and a function that compares its two
    -- arguments and then returns a sorted list

    genericMerge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
    genericMerge x [] _ = x
    genericMerge [] x _ = x
    genericMerge (x:xs) (y:ys) compare
        | compare x y = x : genericMerge xs (y:ys) compare
        | otherwise = y : genericMerge (x:xs) ys compare

    genericSort :: Ord a => [a] -> (a -> a -> Bool) -> [a]
    genericSort [] _ = []
    genericSort [x] _ = [x]
    genericSort list compare = genericMerge (genericSort firstHalf compare) (genericSort secondHalf compare) compare
        where (firstHalf, secondHalf) = splitHalf list

    -- Write a Haskell function to convert a string to a number. The string
    -- should be in the form of $2,345,678.99 and can possibly have leading
    -- zeros.

    stringToNumber :: String -> Float
    stringToNumber x = read withoutDollarAndComma :: Float
        where withoutDollarAndComma = foldl deleteDollarAndComma "" x

    deleteDollarAndComma :: String -> Char -> String
    deleteDollarAndComma l x
        | x == '$' = l
        | x == ',' = l
        | otherwise = l ++ [x]

    -- Write a function that takes an argument x and returns a lazy sequence
    -- that has every third number, starting with x. Then, write a function
    -- that includes every fifth number, beginning with y. Combine these
    -- functions through composition to return every eighth number, beginning
    -- with x + y.

    everyThird :: Num t => t -> [t]
    everyThird x = x:(everyThird (x + 3))

    everyFifth :: Num t => t -> [t]
    everyFifth y = y:(everyFifth (y + 5))

    -- This is no function composition...
    everyEight :: Num t => t -> t -> [t]
    everyEight x y = zipWith (+) (everyThird x) (everyFifth y)

    -- Use a partially applied function to define a function that will return
    -- half of a number and another that will append \n to the end of any
    -- string

    multiply x y = x * y
    half = multiply 0.5

    append x y = y ++ x
    appendNewline = append "\n"

    -- Write a function to determine the greatest common denominator of two
    -- integers.

    -- Use Euclid's algorithm
    greatestCommonDenominator :: Integer -> Integer -> Integer
    greatestCommonDenominator x y
        | x > y = greatestCommonDenominator (x - y) y
        | x < y = greatestCommonDenominator x (y - x)
        | otherwise = x

    -- Create a lazy sequence of prime numbers.

    shareNoCommonDenominator x primesSmaller = all (\y -> (greatestCommonDenominator x y) == 1) primesSmaller
    primesHelper n primesSmaller
        | shareNoCommonDenominator n primesSmaller = n : (primesHelper (n + 1) (n : primesSmaller))
        | otherwise = primesHelper (n + 1) primesSmaller
    allPrimes = primesHelper 1 [1]

    main :: IO()
    main = return ()