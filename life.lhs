Life in Haskell - from Graham Hutton's Book
==========================================

> module Main where

This is a literal Haskell file so I can use free text to describe the program
interleaved with the program itself, look here comes the import!

> import Control.Concurrent.Thread.Delay
> import System.Console.Terminal.Size

> type Pos = (Int,Int)
> type Board = [Pos]

> cls :: IO ()
> cls = putStr "\ESC[2J"

> terminalSize :: IO (Maybe (Window Int)) 
> terminalSize = size

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

> neighbours :: Window Int -> Pos -> [Pos]
> neighbours w (x,y) = map (wrap w) [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

> wrap :: Window Int -> Pos ->  Pos 
> wrap (Window width height) (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

> liveNeighbours :: Window Int -> Board -> Pos -> Int
> liveNeighbours w b = length . filter (isAlive b) . (neighbours w)

> survivors :: Window Int -> Board -> [Pos]
> survivors w b = [p | p <- b, elem (liveNeighbours w b p ) [2,3]]

> births :: Window Int -> Board -> [Pos]
> births w b = [p | p <- rmDupes (concat (map (neighbours w) b)), isEmpty b p, liveNeighbours w b p == 3]

> rmDupes :: Eq a => [a] -> [a]
> rmDupes [] = []
> rmDupes (x:xs) = x : rmDupes (filter (/= x) xs)

> nextGen :: Window Int -> Board -> Board
> nextGen w b = survivors w b ++ births w b

> life :: Maybe (Window Int) -> Board -> IO ()
> life Nothing _ = putStrLn "Cannot determine terminal size!"
> life (Just w) b = do cls;
>             		   showCells b;
>             		   delay 1000;
>             		   life (Just w) (nextGen w b)

> main :: IO ()
> main = do w <- terminalSize;
>			life w glider