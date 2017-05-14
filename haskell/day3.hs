module Main where

    import Data.List
    import Control.Monad

    -- Write a function that looks up a hash table value that uses the Maybe
    -- monad. Write a hash that stores other hashes, several levels deep. Use
    -- the Maybe monad to retrieve an element for a hash key several levels
    -- deep.

    -- The documentation states that Data.Map is a HashMap and it already
    -- returns values using the Maybe monad, so there is nothing to do for the
    -- first part of the exercise... So I ended up implementing my own Map.

    -- A map is a list with 2-tuples, e.g. [(key0, value0), (key1, value1), ...]
    myLookup key [] = Nothing
    myLookup searchedKey ((key, value):xs)
        | searchedKey == key = Just value
        | otherwise = myLookup searchedKey xs

    -- Use the Maybe monad to retrieve an element for a hash key several levels deep
    severalLevelsDeep = myLookup "level1" [("level1", [("level2", "found it")])] >>= myLookup "level2"

    severalLevelsDeepFailOnFirst = myLookup "level2" [("level1", [("level2", "found it")])] >>= myLookup "level2"
    
    -- Represent a maze in Haskell. You'll need a Maze type and a Node type, as
    -- well as a function to return a node given it's coordinates. The node
    -- should have a list of exits to other nodes.

    -- To Solve a maze a goal is needed
    data Node = Goal (Int, Int) | Connected (Int, Int) [Node]

    getCoordinates :: Node -> (Int, Int)
    getCoordinates (Goal (x, y)) = (x, y)
    getCoordinates (Connected (x, y) _) = (x, y)

    -- Equality by coordinates. If the default-eq is used, an endless
    -- recursion starts
    instance Eq Node where
        x == y = getCoordinates x == getCoordinates y

    -- Show only coordinates for the same reason
    instance Show Node where
        show x = show $ getCoordinates x

    -- Use nested Maps to get nodes by coordinates
    -- Because the nodes know their location, the maze itself is not needed
    -- during the search
    type Maze = [(Int, [(Int, Node)])]

    getNodeAtCoordinates :: Maze -> Int -> Int -> Maybe Node
    getNodeAtCoordinates maze x y = myLookup x maze >>= myLookup y

    -- Use a List monad to solve the maze.

    -- Take a maze and a start node and return a list of all nodes of a
    -- solution or an empty list of there is no solution

    solve :: Node -> [(Int, Int)]
    solve start = solveHelper start [start]

    solveHelper :: Node -> [Node] -> [(Int, Int)]
    solveHelper currentNode@(Goal _) currentWay = reverse (map getCoordinates currentWay)
    solveHelper currentNode@(Connected _ exits) currentWay =
        let nodesToExplore = exits \\ currentWay
        in do
            guard $ not $ null nodesToExplore -- if nodesToExplore is empty, backtrack
            nextNode <- nodesToExplore
            solveHelper nextNode (nextNode:currentWay)

    -- For testing
    node1 = Connected (1, 1) [node2, node4]
    node2 = Connected (1, 2) [node1, node3]
    node3 = Connected (1, 3) [node2, node6]
    node4 = Connected (2, 1) [node1, node7]
    node5 = Goal (2, 2)
    node6 = Connected (2, 3) [node5, node3, node9]
    node7 = Connected (3, 1) [node4, node8]
    node8 = Connected (3, 2) [node7]
    node9 = Connected (3, 3) [node6]

    -- Implement a Monad in a nonfunctional language.

    main :: IO()
    main = return ()