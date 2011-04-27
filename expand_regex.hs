module Main where
import Data.List
import Control.Monad
import Control.Monad.Instances

data Regex = Sequence Regex Regex
           | Alteration Regex Regex
           | OneOf [Char]
           | Literal String
           | KleeneStar Regex
           | Plus Regex
           | Optional Regex
           deriving Show


interleave :: [a] -> [a] -> [a]
interleave (a:as) (b:bs) = [a,b] ++ (interleave as bs)
interleave a [] = a
interleave [] b = b

expand :: Regex -> [String]
expand (Literal s) = return s
expand (Alteration a b) = interleave (expand a) (expand b)
expand (OneOf a) = map return a
expand (Optional a) = []:(expand a)
expand (Sequence a b) = liftM2 (++) (expand a) (expand b)
expand (Plus a) = (concat . ap (iterate . liftM2 (++) . expand) expand) a
--expand (Plus a) = foldr interleave [] $ do
--                       match <- expand a
--                       return $ match:(map (match++) (expand (Plus a)))
expand (KleeneStar a) = []:(expand (Plus a))
