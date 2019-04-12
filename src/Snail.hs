module Snail where

import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Array.IO
import Data.Typeable

data Letter = LetterX | LetterY
data Sign = SignPlus | SignMinus
data Instruction = Instruction  {
    instSign :: Sign
    , instLetter :: Letter
    , instDistance :: Int
} deriving (Show)

data SState = SState
    (IOArray Int Int)
    Position
    Instruction
    Int

data Position = Position {
    x :: Int,
    y :: Int
}

data IOArray2D a = IOArray2D (IOArray Int a) (Int,Int)

instance Show Letter where
    show (LetterX) = "x"
    show (LetterY) = "y"

instance Show Sign where
    show (SignPlus) = "+"
    show (SignMinus) = "-"

main :: IO ()
--main =
--    putStr "Input number: "
--    >> getLine
--    >>= \v ->
--    let n = read v :: Int in
--    (newArray (0,n*n-1) 0 :: IO (IOArray Int Int)) >>= \arr ->
--    p arr 0 >>
--    printArray2D arr 0 0 >>
--    return ()
main =
    let n = 5 in
    do
    arr <- (newArray (0,n*n-1) 1 :: IO (IOArray Int Int))

    runStateT
        (moveN (2*n-1))   -- run `move` specified number of times
        (initialState arr n)   -- with initial state.

    printArray2D arr n n
    return ()

main' :: IO ()
main' = do
    -- an intuitive way to look at this:
    -- the <- "strips away" the IO and arr's type is (IOArray Int Int).
    arr <- newArray (0, 10) 0 :: IO (IOArray Int Int)
    
    let a = [0, 1, 2, 3]
    let b = [0, 1, 4, 9]
    let t' = (\t -> writeArray' arr (fst t) (snd t)) <$> (zip a b)
    let xstep = (\(x,y) -> (x+1,y))
    let ystep = (\(x,y) -> (x,y+1))
    foldl (>>) (return ()) t'

    arr2 <- newArray2D (10,20) 0
    writeArray2D arr2 (1,1) 42

    writeArraySeq arr2 (0,0) xstep [1,2,3,4,5]

    --printArray2D arr 10 1

    return ()

initialState arr n =
    SState
        arr
        (Position (-1) 0)
        (Instruction SignPlus LetterX n)
        1

--
-- dim : tuple of width and height (the dimension)
-- iv: initialization value for the allocated array
newArray2D :: (Int,Int) -> a -> IO (IOArray2D a)
newArray2D dim iv = 
    let alloc_end_ix = (fst dim) * (snd dim) - 1 in
    newArray (0, alloc_end_ix) iv >>= \arr -> return $ IOArray2D arr dim
    
writeArray2D :: (IOArray2D a) -> (Int,Int) -> a -> IO ()
writeArray2D arr (x,y) wval =
    let IOArray2D basearr (w,h) = arr
        i = x + (y * w)
    in
    writeArray basearr i wval-- >> return arr

writeArraySeq :: (IOArray2D a)
              -> (Int,Int)
              -> ((Int,Int) -> (Int,Int)) -- given current location, return next location.
              -> [a]
              -> IO ()
writeArraySeq arr initpos f vs =
    let is = trailApply f initpos (length vs) -- indices
        t = (uncurry $ writeArray2D arr) <$> zip is vs
    in
    foldl (>>) (return ()) t

trailApply :: (a -> a) -> a -> Int -> [a]
trailApply f i 0 = []
trailApply f i c = f i : (trailApply f (f i) (c-1))
    

readArray2D :: (IOArray2D a) -> (Int,Int) -> IO a
readArray2D arr (x,y) =
    let IOArray2D basearr (w,h) = arr
        i = x + (y * w)
    in
    readArray basearr i


moveN :: Int -> StateT SState IO ()
moveN n = foldl (>>) (return ()) (replicate n move)
    
move :: StateT SState IO ()
move = do
    SState arr pos inst n <- get
    let sign = instSign inst
    let letter = instLetter inst
    let d = instDistance inst

    let valmask = (+n) <$> [0 .. (d-1)]
    let offmask = indexify <$> (fx pos sign letter) <$> [1 .. d]
    let endpos = fx pos sign letter d
    
    let t = (uncurry $ writeArray' arr) <$> (zip offmask valmask)
    liftIO $ foldl (>>) (return ()) t

    let inst' = instruction_next inst
    put (SState arr (Position (fst endpos) (snd endpos)) inst' (n + d))

    --liftIO $ putStrLn $ show $ instLetter inst
    --liftIO $ putStrLn $ show valmask
    --liftIO $ putStrLn $ show offmask
    --return ()

writeArray' :: (IOArray Int Int) -> Int -> Int -> IO ()
writeArray' arr i e = writeArray arr i e

indexify :: (Int, Int) -> Int
indexify p =
    let x = fst p
        y = snd p in
    x + (y * 5)

fx :: Position -> Sign -> Letter -> Int -> (Int,Int)
fx pos SignPlus  LetterX off = ((x pos) + off, y pos)
fx pos SignMinus LetterX off = ((x pos) - off, y pos)
fx pos SignPlus  LetterY off = (x pos        , (y pos) + off)
fx pos SignMinus LetterY off = (x pos        , (y pos) - off)

instruction_next :: Instruction -> Instruction
instruction_next (Instruction s LetterX n) = Instruction s LetterY (n-1)
instruction_next (Instruction SignPlus LetterY n) = Instruction SignMinus LetterX n
instruction_next (Instruction SignMinus LetterY n) = Instruction SignPlus LetterX n

printArray2D :: (IOArray Int Int) -> Int -> Int -> IO ()
printArray2D arr w h = do
    f <- sequence $ (getArrayRow arr w) <$> [0 .. (h-1)]
    print f

getArrayRow :: (IOArray Int Int) -> Int -> Int -> IO ([Int])
getArrayRow arr d y = 
    let mask = (+(y * d)) <$> [0,1 .. (d-1)] in
    do
        sequence $ (readArray' arr) <$> mask

readArray' :: (IOArray Int Int) -> Int -> IO (Int)
readArray' arr i = readArray arr i




-- [(+)1, (+)2, (+)3] >>= (\f -> return $ f 1)
-- [1, 2, 3] >>= (\n -> return $ (+)1 n)
-- fmap ((+)1) [1,2,3]
