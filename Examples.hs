import HLogic
import Control.Monad hiding (fail)


-- To run examaple:
-- in repl "run example1"
-- or "take 1 $ run $ sudoku board2"

example1 = do
    a <- fresh
    b <- fresh
    membero a ([1,2,3,4] :: [Int])
    membero b ([3,4,5,6] :: [Int])
    a === b
    return a
-- => [3,4]

example2 = do
    a <- fresh
    b <- fresh
    membero a ([1,2,3] :: [Int])
    membero b ([2,3,4] :: [Int])
    a =/= b
    c <- fresh
    c === [a,b]
    return c
-- => [[1, 2],[1, 3],[1, 4],[2, 3],[2, 4],[3, 2],[3, 4]]

example3 = do
    a <- fresh
    b <- fresh
    [TInt 1, a] === [b, TInt 2]
    return [a,b]
-- => [[2, 1]]

example4 = do
    q <- fresh
    w <- fresh
    appendo q w (toTerm [1,2,3,4 :: Int])
    return [q,w]
-- => [[[], [1, 2, 3, 4]],[[1], [2, 3, 4]],[[1, 2], [3, 4]],[[1, 2, 3], [4]],[[1, 2, 3, 4], []]]


example5 = do
	[a,b,c,d,e] <- replicateM 5 fresh
	[a,b,c,d] === [a, a, TInt 3, c]
	return [a,b,c,d,e]
-- => [[?_0, ?_0, 3, 3, ?_4]]


relation1 tab a = msum [a === x | x <- tab]
relation2 tab a b = msum [[a,b] === x | x <- tab]
parent = relation2 [["John", "Alice"], ["George", "John"], ["Lucy", "John"]]
male = relation1 ["George", "John"]
child x y = parent y x
son x y = (child x y) >> (male x)
grandparent x y = do
    p <- fresh
    x `parent` p
    p `parent` y
grandpa x y = (grandparent x y) >> (male x) 

genealogy1 = do
    q <- fresh
    grandparent q (toTerm "Alice")
    return q
-- => [George,Lucy]

genealogy2 = do
    q <- fresh
    grandpa q (toTerm "Alice")
    return q
-- => [George]

genealogy3 = do
	q <- fresh
	w <- fresh
	son q w
	return [q,w]
-- => [[John, George],[John, Lucy]]


board1 =
    [[4,2,0,0],
     [0,0,0,0],
     [0,0,0,0],
     [0,0,0,0]] :: [[Int]]

board2 =
    [[0,0,3, 0,2,0, 6,0,0],
     [9,0,0, 3,0,5, 0,0,1],
     [0,0,1, 8,0,6, 4,0,0],

     [0,0,8, 1,0,2, 9,0,0],
     [7,0,0, 0,0,0, 0,0,8],
     [0,0,6, 7,0,8, 2,0,0],

     [0,0,2, 6,0,9, 5,0,0],
     [8,0,0, 2,0,3, 0,0,9],
     [0,0,5, 0,1,0, 3,0,0]] :: [[Int]]

distrincto vars = sequence_ [a =/= b | a <- vars, b <- vars, a /= b]
domain col var = msum [var === x | x <- col]

initBoard board rows =
    sequence_ [ var === val | x <- [0..(size-1)], y <- [0..(size-1)], 
                              let val = (board !! x) !! y,
                              val /=0,
                              let var = (rows !! x) !! y ]
        where size = length rows

sudoku board = do
    let size = length board
    let sqrSize = floor $ sqrt $ fromIntegral size
    let cells = size * size
    
    vars <- replicateM cells fresh  -- one variable for each cell
    let rows = [[ vars !! (x * size + y) | y <- [0..(size-1)]] | x <- [0..(size-1)]]
    let cols = [[ vars !! (x * size + y) | x <- [0..(size-1)]] | y <- [0..(size-1)]]
    let sqrs = [[ vars !! ((sqrSize * sx + x) * size + y + sqrSize * sy) | x <- [0..(sqrSize-1)],
                                                                           y <- [0..(sqrSize-1)]]
                                                                         | sx <- [0..(sqrSize-1)],
                                                                           sy <- [0..(sqrSize-1)]]
    let numbers = map toTerm ([1..size] :: [Int])  -- possible numbers

    initBoard board rows   -- set constrains for initial clues
    mapM_ distrincto rows  -- each row contains distrinct numbers
    mapM_ distrincto cols
    mapM_ distrincto sqrs
    mapM_ (domain numbers) vars
    return rows
