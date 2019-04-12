 {-# LANGUAGE ScopedTypeVariables #-}
module RandomTest where

import System.Random
import Control.Monad
import Control.Applicative

roll :: State StdGen Double
roll = do
    generator <- get
    let (value, generator') = random generator :: (Double, StdGen)
    put generator'
    return value

roll' :: State StdGen Double
roll' = 
    get >>= (\generator ->
        let (value, generator') = random generator :: (Double, StdGen) in
        (put generator') >>= (\_ ->
            return value
        )
    )

rollMulti :: State StdGen [Double]
rollMulti = do
    v1 <- roll
    v2 <- roll
    v3 <- roll
    return $ [v1, v2, v3]

rollMulti' :: [Double]
rollMulti' = 
    fst $ runState (do
        v1 <- roll
        v2 <- roll
        v3 <- roll
        return [v1,v2,v3]
    ) (mkStdGen 1234)

f :: State StdGen (Double,Double)
f = liftA2 (,) roll roll

-- runState rollMulti (mkStdGen 42)

rollNDice :: Int -> State StdGen [Double]
rollNDice n = replicateM' n roll

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n randomIO

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- sequence' :: Monad m => [m a] -> m [a]

replicateM' n x = sequence $ replicate' n x

-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
-- Now, how the hell is `sequence` implemented?
