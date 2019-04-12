module StateFromScratch where

import Data.Char

rmspaces :: String -> String
rmspaces [] = ""
rmspaces (x:xs)
    | x == ' ' = rmspaces xs
    | otherwise = [x] ++ rmspaces xs

tolower :: String -> String
tolower s = toLower <$> s

rmspaces' :: String -> Int -> (String, Int)
rmspaces' xs c =
    let rmspaced = rmspaces xs in
    (,) rmspaced (length rmspaced + c)

tolower' :: String -> Int -> (String, Int)
tolower' s c =
    let p1 = toLower <$> s
        p2 = length s + c
    in
        (,) p1 p2

-- the "compose" function
(><) :: (a -> s -> (b, s)) -> (b -> s -> (c, s)) -> (a -> s -> (c, s))
(><) f g x s =
    let (r, s') = f x s in
    g r s'

-- the "bind" function
bind :: (s -> (a,s)) -> (a -> s -> (b,s)) -> (s -> (b,s))
bind f l s =
    let (r,s') = f s in
    l r s
    
hyperid :: a -> s -> (a, s)
hyperid x s = (x, s)

hyperize :: (a -> b) -> (a -> s -> (b, s))
hyperize f x s = (f x, s)

-- matches the s -> (a, s) shape such that it puts the current (unchanged)
-- state into both slots (the 'value' and the 'state' slots):
get :: s -> (s, s)
get s = (s, s)

-- feeds the state through a function, and fills the 'value' slot with the
-- least meaningful possible value, ():
modify :: (s -> s) -> s -> ((), s)
modify f s = ((), f s)

-- takes a new state and an old state, discards the old state, and returns
-- a similar pair of () and the new state:
put :: s -> s -> ((), s)
put s' s = ((), s')


-- (get `bind` (\n -> (rmspaces' "hi "))) 0
