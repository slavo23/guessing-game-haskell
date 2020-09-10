module Main where

import           Control.Monad
import           Data.List
import           System.Random

data GameState = GameState
    { guessed     :: String
    , transformed :: String
    , indices     :: [Int]
    , attempts    :: Int
    , limit       :: Int
    , difficulty  :: Int
    }

space :: Char
space = '_'

getRandomIds :: (Int, Int) -> Int -> IO [Int]
getRandomIds range amt = nub <$> (replicateM amt $ randomRIO range)

replaceAt :: String -> Int -> Char -> String
replaceAt [] at char = []
replaceAt (x:xs) at char | at == 0   = char : xs
                         | otherwise = x : replaceAt xs (at - 1) char

replaceManyWith :: String -> Char -> [Int] -> String
replaceManyWith str c []     = str
replaceManyWith str c (x:xs) = replaceManyWith (replaceAt str x c) c xs

fillRandomWhiteSpace :: String -> [Int] -> String
fillRandomWhiteSpace str list = replaceManyWith str space list

createState :: String -> Int -> [Int] -> GameState
createState word tries rs = GameState{ guessed = word
                                      , transformed  = fillRandomWhiteSpace word rs
                                      , indices      = rs
                                      , attempts     = 0
                                      , limit        = tries
                                      , difficulty   = length rs
                                     }

updateState :: GameState -> Char -> GameState
updateState st c = case (indexOf c $ guessed st) of
                        Nothing -> failure
                        Just i  -> if i `elem` (indices st)
                                   then success i
                                   else failure
                        where indexOf x xs = elemIndex x xs
                              failure      = st{attempts = attempts st + 1}
                              success x    = st{transformed = replaceAt (transformed st) x c}

gameLoop :: GameState -> IO ()
gameLoop st | attempts st == limit st       = print $ "You lost after " ++ (show $ attempts st) ++ "attempts" 
            | guessed  st == transformed st = print $ "You're right, the word is " ++ guessed st
            | otherwise = do print    $ transformed st
                             guess    <- liftM head $ getLine
                             gameLoop $ updateState st guess

main :: IO ()
main = do
       let word  = "haskell"
       idx       <- getRandomIds (0, length word - 1) 4
       let state = createState word 5 idx
       gameLoop state
