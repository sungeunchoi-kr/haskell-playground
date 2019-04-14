module Scramble where

import Control.Monad.State
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

nextRandRange :: Int -> State StdGen Int
nextRandRange n = state (\seed -> 
    let (a,g) = randomR (0,n) seed in (a,g))

takeout :: Int -> [a] -> ([a], a)
takeout i xs = 
    let (ys,zs) = splitAt i xs in
    (ys ++ (tail zs), xs !! i)

performN :: Monad m => Int -> m a -> m [a]
performN 0 _ = return []
performN n m = do
    a1 <- m
    a2 <- performN (n-1) m
    return (a1 : a2)

scrambleStep :: StateT [a] (State StdGen) a
scrambleStep = do
    as <- get
    r <- lift $ nextRandRange (length as - 1)
    let (as',a) = takeout r as
    put as'
    return a

scramble :: StateT [a] (State StdGen) [a]
scramble = do
    as <- get
    arr <- performN (length as) scrambleStep
    return arr

runScramble l seed = evalState (evalStateT scramble l) seed

main :: IO ()
main = do
    content <- lines <$> readFile "list.txt"
    seed <- (round . (* 1000)) <$> getPOSIXTime 
    let scrambledList = runScramble content (mkStdGen seed)
    putStrLn $ "seed = " ++ (show seed)
    printFormatList scrambledList

printFormatList :: [String] -> IO ()
printFormatList [] = return()
printFormatList (x:xs) = putStrLn x >> printFormatList xs
    
