Life in Haskell - from Graham Hutton's Book
===========================================

Warn about tabs

> {-# OPTIONS -Wall #-} 

> module Main where

This is a literate Haskell file so I can use free text to describe the program
interleaved with the program itself, look here comes the import!

I'm porting the IO version from the book to ncurses for a nicer UI.

> import Control.Concurrent.Thread.Delay
> import UI.NCurses
> import Control.Monad.IO.Class

> type Pos = (Integer,Integer)
> type Board = [Pos]

> columns :: Integer
> columns = 40

> rows :: Integer
> rows = 20

> glider :: Board
> glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

> writeAt :: Pos -> String -> Update ()
> writeAt (x,y) s = do 
>                     moveCursor y x
>                     drawString s

moveCursor is row, column which is why the x, y are inverted

> isAlive :: Board -> Pos -> Bool
> isAlive b p = elem p b

> isEmpty :: Board -> Pos -> Bool
> isEmpty b p = not $ isAlive b p

> neighbours :: Pos -> [Pos]
> neighbours (x,y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

> wrap :: Pos ->  Pos 
> wrap (x,y) = ((x `mod` columns), (y `mod` rows)) 

The glider still crashes with: life: CursesException "drawString: rc == ERR"
I thought it was that my wrap function was generating dodgy out-of-bounds positions (it was before) but the following function
returns True

> verifyWrapIsOk :: Bool
> verifyWrapIsOk = [] == ((filter outOfBounds) $ map wrap [(x,y) | x <- [-100..100], y <- [-100..100]])
>        	   where
>				 outOfBounds (x,y) = x < 0 || x > (columns-1) || y < 0 || y > (rows-1)

i.e. for all positions including much greater than the bounds of the board the wrapped values all exist within the board

> liveNeighbours :: Board -> Pos -> Int
> liveNeighbours b = length . filter (isAlive b) . neighbours

> survivors :: Board -> [Pos]	
> survivors b = [p | p <- b, elem (liveNeighbours b p) [2,3]]

> births :: Board -> [Pos]
> births b = [p | p <- rmDupes (concat (map neighbours b)), isEmpty b p, liveNeighbours b p == 3]

> rmDupes :: Eq a => [a] -> [a]
> rmDupes [] = []
> rmDupes (x:xs) = x : rmDupes (filter (/= x) xs)

> nextGen :: Board -> Board
> nextGen b = survivors b ++ births b

> cls :: Update ()
> cls = do 
>         sequence_ [writeAt (x,y) " " | x <- [1..(columns-2)], y <- [1..(rows-2)]]

Note that cls doesn't overwrite the border we drew

> showCells :: Window -> Board -> Integer -> Curses () 
> showCells w b n = updateWindow w $ do 
>                     resizeWindow rows columns
>                     drawBox Nothing Nothing
>                     cls
>                     sequence_ [writeAt p "0" | p <- b]
>                     moveCursor 0 0
>                     drawString $ "frame: " ++ (show n)

> life :: Window -> Board -> Integer -> Curses ()
> life w b n = do 
>                showCells w b n
>                render
>                liftIO $ delay 100000
>                life w (nextGen b) (n+1)

> main :: IO ()
> main = runCurses $ do 
>          setEcho False 
>          w <- defaultWindow 
>          life w glider 1
