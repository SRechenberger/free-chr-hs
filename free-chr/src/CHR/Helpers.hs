module CHR.Helpers where

import Control.Applicative (liftA2)

infixr 3 &&.
(&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&.) = liftA2 (&&)

infixr 2 ||.
(||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(||.) = liftA2 (||)

between :: Ord a => a -> a -> a -> Bool
between l u = liftA2 (&&) (l <=) (<= u)