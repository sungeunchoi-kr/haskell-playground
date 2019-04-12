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

scrambleStep :: StateT ([a],[a]) (State StdGen) ()
scrambleStep = do
    (as,rs) <- get
    r <- lift $ nextRandRange (length as - 1)
    let (as',a) = takeout r as
    put (as',rs++[a])

scramble :: StateT ([a],[a]) (State StdGen) ([a])
scramble = do
    (as,_) <- get
    foldl (>>) (return ()) (replicate (length as) scrambleStep)
    (_,rs) <- get
    return rs

runScramble l seed = evalState (evalStateT scramble (l,[])) seed

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
    
