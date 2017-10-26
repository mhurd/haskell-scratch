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
> import qualified Data.ByteString as B
> import qualified Data.Text as T
> import Data.Text.Encoding (encodeUtf8)

> mkStr :: String -> B.ByteString
> mkStr = encodeUtf8 . T.pack

> type Pos = (Integer,Integer)
> type Board = [Pos]

> outputFilename :: String
> outputFilename = "life-output.txt"

> displayColumns :: Integer
> displayColumns = 40

> displayRows :: Integer
> displayRows = 20

> glider :: Board
> glider = [(6,2),(4,3),(6,3),(5,4),(6,4)]

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
> wrap (x,y) = ((x `mod` displayColumns), (y `mod` displayRows)) 

The glider still crashes with: life: CursesException "drawString: rc == ERR"
I thought it was that my wrap function was generating dodgy out-of-bounds positions (it was before) but the following function
returns True

> verifyWrapIsOk :: [Pos]
> verifyWrapIsOk = ((filter outOfBounds) $ map wrap [(x,y) | x <- [0..displayColumns], y <- [0..displayRows]])
>        	   where
>				 outOfBounds (x,y) = x <= 0 || x >= displayColumns-2 || y <= 0 || y >= displayRows-2

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
>         sequence_ [writeAt (x,y) " " | x <- [1..(displayColumns-2)], y <- [1..(displayRows-2)]]

Note that cls doesn't overwrite the border we drew

> showCells :: Window -> Board -> Integer -> Curses ()
> showCells w b n = updateWindow w $ do 
>                     resizeWindow displayRows displayColumns
>                     drawBox Nothing Nothing
>                     cls
>                     sequence_ [writeAt p "0" | p <- b]
>                     moveCursor 0 0
>                     drawString $ "frame: " ++ (show n)

> showDone :: Window -> Curses ()
> showDone w = updateWindow w $ do 
>                resizeWindow displayRows displayColumns
>                moveCursor 0 0
>                drawString $ "Reached max generations!"

> writeToFile :: Integer -> Board -> IO ()
> writeToFile n b = B.appendFile outputFilename (mkStr $ "Gen " ++ (show n) ++ " = " ++ (show b) ++ "\n")

> life :: Window -> Board -> Integer -> Curses ()
> life w b 500 = do 
>                   liftIO $ writeToFile 500 b
>                   showCells w b 500
>                   showDone w
>                   render
>                   liftIO $ delay 2000000

> life w b n = do 
>                liftIO $ writeToFile n b
>                showCells w b n
>                render
>                liftIO $ delay 100000
>                life w (nextGen b) (n+1)

> main :: IO ()
> main = runCurses $ do 
>          liftIO $ B.writeFile outputFilename (mkStr "Life!\n=====\n")
>          setEcho False 
>          w <- defaultWindow
>          life w glider 1