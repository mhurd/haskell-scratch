module Main where

import Control.Monad (forever, when)
import Data.Char (toLower, ord)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: "
            ++ guessed ++ " Guesses remaining: " ++ show (maxGuesses - length guessed)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

maxGuesses :: Int
maxGuesses = 11

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle word
    runGame puzzle

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character"

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar guessed) =
    when (length guessed == maxGuesses && not (all isJust filledInSoFar)) $
        do
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    when (all isJust filledInSoFar) $
        do putStrLn "You win!"
           exitSuccess

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling accordingly."
            return (fillInCharacter puzzle guess)
        (false, _) -> do
            putStrLn "The character wasn't in the word, please try again."
            return (fillInCharacter puzzle guess)

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  let filtered = filter gameFilter aw
  return (map (fmap toLower) filtered)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength
             && l < maxWordLength
        alphaChar c =
            ord (c :: Char) >= 97 && ord (c :: Char) <= 122
        alpha w =
            all alphaChar (w :: String)
        gameFilter w =
            gameLength w && alpha w

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord