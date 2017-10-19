Life in Haskell - from Graham Hutton's Book
===========================================

> module Main where

This is a literate Haskell file so I can use free text to describe the program
interleaved with the program itself, look here comes the import!

I'm porting the IO version from the book to ncurses for a nicer UI.

> import Control.Concurrent.Thread.Delay
> import UI.NCurses

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

> isAlive :: Board -> Pos -> Bool
> isAlive b p = elem p b

> isEmpty :: Board -> Pos -> Bool
> isEmpty b p = not $ isAlive b p

> neighbours :: Pos -> [Pos]
> neighbours (x,y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

> wrap :: Pos ->  Pos 
> wrap (x,y) = (((x-1) `mod` columns), ((y-1) `mod` rows))

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

> cls :: Update ()
> cls = do 
>         sequence_ [writeAt (x,y) " " | x <- [0..(columns-2)], y <- [0..(rows-2)]]

> showCells :: Window -> Board -> Int -> Curses () 
> showCells w b n = updateWindow w $ do 
>                     resizeWindow rows columns
>                     cls
>                     drawBox Nothing Nothing
>                     sequence_ [writeAt p "*" | p <- b]
>                     moveCursor 0 0

> life :: Window -> Board -> Int -> Curses ()
> life w b n = do 
>                showCells w b n
>                render
>                life w (nextGen b) (n+1)
>                liftIO $ delay 500

I can't work out how to get the call out to delay :: Integer -> IO ()
to work? I get this on compilation: 

life.lhs:74:18: error:
    Variable not in scope: liftIO :: IO () -> Curses ()

I thought liftIO was supposed to 'lift' the IO Monad into the Curses Monad? Curses
has a MonadIO instance: https://hackage.haskell.org/package/ncurses-0.2.16/docs/UI-NCurses.html 

> main :: IO ()
> main = runCurses $ do 
>          setEcho False 
>          w <- defaultWindow 
>          life w glider 1
