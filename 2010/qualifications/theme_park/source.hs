module Main where

import qualified Data.Set as S
import Control.Monad (foldM_)
import Data.List (foldl')
import Debug.Trace

sum' :: [Integer] -> Integer
sum' = foldl' (+) 0

select :: (a -> Integer) -> Integer -> [a] -> ([a],[a])
select _ _ [] = ([],[])
select f k (x0:xs) | k<0       = ([],x0:xs)
                   | otherwise = let x       = f x0
                                     (ys,zs) = select f (k-x) xs
                                 in if (x<=k)
                                    then (x0:ys,zs)
                                    else (ys,x0:xs)

partition :: Integer -> [Integer] -> ([[Integer]],[[Integer]])
partition k = finalize . partition_ . zip [0..]
  where finalize xss0 = let point     = last xss0
                            preperiod = takeWhile (/=point) xss0
                            period    = dropWhile (/=point) xss0
                            seconds   = map (map snd)
                        in (seconds preperiod,init $ seconds period)

        partition_ = nub . recurse . select snd k
          where recurse ([],next)    = partition_ next
                recurse (group,next) = group : partition_ (next ++ group)
        
                nub []       = []
                nub (xs:xss) = xs : nub (remove xs xss)
                  where remove _ []                    = []
                        remove zs (ys:yss) | zs==ys    = [ys]
                                           | otherwise = ys : remove zs yss

runCoaster :: Integer -> Integer -> [Integer] -> Integer
runCoaster r k = earnings . partition k
  where earnings (xs0,ys0) = let xs      = map sum' xs0
                                 ys      = map sum' ys0
                                 r_      = r - fromIntegral (length xs)
                                 minimum = fromIntegral (length ys)
                                 (q,b)   = r_ `divMod` minimum
                             in sum' xs + sum' ys * q + sum' (take (fromIntegral b) ys)

-- runCoaster2 :: Integer -> Integer -> [Integer] -> Integer
-- runCoaster2 0 _ _  = 0
-- runCoaster2 r k ps = let (run,next) = select id k ps
--                      in sum' run + runCoaster2 (r-1) k (next ++ run)

main :: IO ()
main = do (t:ts) <- fmap lines getContents
          foldM_ exec 1 (take (read t) (group2 ts))
  where exec c (x,y) = let [r,k,n] = map read . words $ x
                           ps      = take (fromIntegral n) (map read (words y))
                       in do putStr "Case #"
                             putStr (show c) 
                             putStr ": "
                             putStrLn (show $ runCoaster r k ps)
                             return (c+1)
        
        group2 (x:y:ts) = (x,y) : group2 ts
        group2 _        = []

