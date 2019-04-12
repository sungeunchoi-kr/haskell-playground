module Main where

import qualified Control.Monad.State as Monad
import qualified Control.Monad.Writer as Monad
import qualified Control.Monad.Reader as Monad

type GameState = (Bool, Int)

playGame :: String -> Monad.State GameState Int
playGame []     = do
    (_, score) <- Monad.get
    return score

playGame (x:xs) = do
    (on, score) <- Monad.get   -- MonadState s m => m s
    case x of
         'a' | on -> Monad.put (on, score + 1)
         'b' | on -> Monad.put (on, score - 1)
         'c'      -> Monad.put (not on, score)
         _        -> Monad.put (on, score)
    playGame xs

startState = (False, 0)

main :: IO ()
main = do
    -- recall: evalState :: State s a -> s -> a
    print $ Monad.evalState (playGame "abcaaacbbcabbab") startState
--                          ^--------- State s a ------^ ^-- s ---^
    print "end"
    return ()

