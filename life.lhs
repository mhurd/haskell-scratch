## Life in Haskell - from Graham Hutton's Book

> module Main where

This is a literal Haskell file so I can use free test to describe the program
interleaved with the program itself, look her comes the import!

> import Control.Concurrent.Thread.Delay

> type Pos = (Int,Int)
> type Board = [Pos]

> cls :: IO ()
> cls = putStr "\ESC[2J"

> width :: Int
> width = 20

> height :: Int
> height = 20

> writeAt :: Pos -> String -> IO ()
> writeAt p xs = do goto p
>                   putStr xs

> goto :: Pos -> IO ()
> goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

> glider :: Board
> glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

> showCells :: Board -> IO () 
> showCells b = sequence_ [writeAt p "0" | p <- b]

> isAlive :: Board -> Pos -> Bool
> isAlive b p = elem p b

> isEmpty :: Board -> Pos -> Bool
> isEmpty b p = not $ isAlive b p

> neighbours :: Pos -> [Pos]
> neighbours (x,y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

> wrap :: Pos -> Pos 
> wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

> liveNeighbours :: Board -> Pos -> Int
> liveNeighbours b = length . filter (isAlive b) . neighbours

> survivors :: Board -> [Pos]
> survivors b = [p | p <- b, elem (liveNeighbours b p ) [2,3]]

> births :: Board -> [Pos]
> births b = [p | p <- rmDupes (concat (map neighbours b)), isEmpty b p, liveNeighbours b p == 3]

> rmDupes :: Eq a => [a] -> [a]
> rmDupes [] = []
> rmDupes (x:xs) = x : rmDupes (filter (/= x) xs)

> nextGen :: Board -> Board
> nextGen b = survivors b ++ births b

> life :: Board -> IO ()
> life b = do cls
>             showCells b
>             delay 1000
>             life (nextGen b)

> main :: IO ()
> main = do life glider
