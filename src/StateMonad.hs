module StateMonad where

import Control.Monad

-- newtype State s a = State {
--     runState :: s -> (a,s)
-- }

--class Monad m where
--    return ::   a                 -> m a
--    (>>=)  :: m a -> (  a -> m b) -> m b
--    (>>)   :: m a ->  m b         -> m b
--    fail   :: String -> m a

--instance Monad (State s) where
--    return x = State { runState = (\t -> (x,t)) }
--    State act (>>=) k = State $ \s ->
--        let (a, s') = act s 
--        in runState (k ) s'

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap = Control.Monad.liftM

instance Applicative (State s) where
    pure = return
    (<*>) = Control.Monad.ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  State act >>= k = State $ \s ->
    let (a, s') = act s
    in runState (k a) s'

-- invariant 1: We need to return State (\s -> (a,s))


logInc :: Int -> (String, Int)
logInc c
    | c `mod` 5 == 0 = ("foo", c+1)
    | otherwise = ("bar", c+1)

stateIntString :: State Int String
stateIntString = State logInc

main :: IO ()
main = do
    print "end"
    return ()
