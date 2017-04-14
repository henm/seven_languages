module Main where

    -- How many different ways can you find to write allEvent?

    allEven :: [Integer] -> [Integer]
    allEven [] = []
    allEven (h:t) = if even h then h:allEven t else allEven t

    allEvenGuard :: [Integer] -> [Integer]
    allEvenGuard numbers = [x | x <- numbers, even x]

    allEvenFilter :: [Integer] -> [Integer]
    allEvenFilter numbers = filter even numbers

    -- Write a function that takes a list and returns the same list in reverse

    ownReverse :: [a] -> [a]
    ownReverse [] = []
    ownReverse (h:t) = ownReverse t ++ [h]

    -- Write a function that builds two-tuples with all possible combinations
    -- of two of the colors black, white, blue, yellow, and red. Note that you
    -- should include only one of (black, blue) and (blue, black).

    data Color = Black | White | Blue | Yellow | Red deriving (Show, Eq, Ord)
    allColors :: [Color]
    allColors = [Black, White, Blue, Yellow, Red]
    buildColorsPairs :: () -> [(Color, Color)]
    buildColorsPairs color = [(x, y) | x <- allColors, y <- allColors, x <= y]

    -- Write a list comprehension to build a childhood multiplication table.
    -- The table would be a list of thee-tuples where the first two are
    -- integers from 1--12 and the third is the product of the first two

    multiplicationTable ::  [(Int, Int, Int)]
    multiplicationTable = [(x, y, x * y) | x <- [1..12], y <- [1..12]]

    -- Solve the map-coloring problem using Haskell.

    -- Strange naming so it doesn't conflict with data Color from above
    data ThreeColor = ThreeRed | ThreeGreen | ThreeBlue deriving (Show, Eq)
    data State = Alabama | Mississippi | Georgia | Tennessee | Florida deriving (Show)

    threeColors = [ThreeRed, ThreeGreen, ThreeBlue]

    different = [((Alabama, a), (Mississippi, m), (Georgia, g), (Tennessee, t), (Florida, f))
        | a <- threeColors, m <- threeColors, g <- threeColors, t <- threeColors, f <- threeColors,
            m /= t, m /= a, a /= t, a /= m, a /= g, a/= f, g /= f, g /= t]


    main :: IO()
    main = return ()