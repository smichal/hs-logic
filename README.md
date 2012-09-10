Logic Programming in Haskell
====

A simple library for Haskell that allows relational programming. It is made for educational purpose.


References
----
* [core.logic](https://github.com/clojure/core.logic)
* [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://pqdtopen.proquest.com/#abstract?dispub=3380156)
* [logict](http://hackage.haskell.org/package/logict)

Some examples...
----
...more examples in `Examples.hs`


```haskell
do
    a <- fresh
    b <- fresh
    membero a ([1,2,3] :: [Int])
    membero b ([2,3,4] :: [Int])
    a =/= b
    c <- fresh
    c === [a,b]
    return c

=> [[1, 2],[1, 3],[1, 4],[2, 3],[2, 4],[3, 2],[3, 4]]
```

```haskell
do
	q <- fresh
	w <- fresh
	appendo q w (toTerm [1,2,3,4 :: Int])
	return [q,w]

=> [[[], [1, 2, 3, 4]],[[1], [2, 3, 4]],[[1, 2], [3, 4]],[[1, 2, 3], [4]],[[1, 2, 3, 4], []]]
```

```haskell
do
	[a,b,c,d,e] <- replicateM 5 fresh
	[a,b,c,d] === [a, a, TInt 3, c]
	return [a,b,c,d,e]

=> [[?_0, ?_0, 3, 3, ?_4]]
```

Usage
----

Logic formulas are represented as monadic computations in Monad(Plus) `MLogic`.
`bind` corresponds to conjunction, `mplus` to disjunction.
`fresh :: MLogic LVar` introduces new variable.
`(===) :: (Termable a, Termable b) => a -> b -> Predicate` succeeds if `a` unifies with `b`.


To get results use `run :: (Termable a) => MLogic a -> [Term]` function, it returns lazy list of possible solutions.

`(=/=)` introduces disequality constrain.
`conso a b c` succeeds if `a` cons `b` equals `c`.
`success` always succeeds.
`fail` never succeeds.
`sth` unifies with everything.
`membero, heado, tailo, emptyo, appendo` lists precicates.

More in sources ;).

Example: Sudoku
----

Declarative description of correct sudoku solution:

```haskell
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
```

Solve Sudoku!

```haskell
*Main> board1
[[4,2,0,0],
 [0,0,0,0],
 [0,0,0,0],
 [0,0,0,0]]
*Main> take 1 $ run $ sudoku board1
[[[4, 2, 1, 3],
  [1, 3, 2, 4],
  [2, 4, 3, 1],
  [3, 1, 4, 2]]]
```

```haskell
*Main> board2
[[0,0,3, 0,2,0, 6,0,0],
 [9,0,0, 3,0,5, 0,0,1],
 [0,0,1, 8,0,6, 4,0,0],
  
 [0,0,8, 1,0,2, 9,0,0],
 [7,0,0, 0,0,0, 0,0,8],
 [0,0,6, 7,0,8, 2,0,0],

 [0,0,2, 6,0,9, 5,0,0],
 [8,0,0, 2,0,3, 0,0,9],
 [0,0,5, 0,1,0, 3,0,0]]
*Main> take 1 $ run $ sudoku board2
[[[4,8,3, 9,2,1, 6,5,7],
  [9,6,7, 3,4,5, 8,2,1],
  [2,5,1, 8,7,6, 4,9,3],

  [5,4,8, 1,3,2, 9,7,6], 
  [7,2,9, 5,6,4, 1,3,8], 
  [1,3,6, 7,9,8, 2,4,5], 
  
  [3,7,2, 6,8,9, 5,1,4], 
  [8,1,4, 2,5,3, 7,6,9], 
  [6,9,5, 4,1,7, 3,8,2]]]
```

How many 4x4 Sudokus are there?

```haskell
*Main> length $ run $ sudoku (replicate 4 (replicate 4 (0 :: Int)))
288
```