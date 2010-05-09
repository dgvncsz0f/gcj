module Main where

import Data.List (foldl',sort)
import Control.Monad (foldM_)

gcds :: (Integral n) => [n] -> n
gcds = foldl' fixed_gcd 0
  where fixed_gcd 0 0 = 0
        fixed_gcd a b = gcd a b

apocalypse :: [Integer] -> Integer
apocalypse xs0 = solve . gcds $ diff
  where  (x:xs) = sort xs0

         diff = map (abs.(`subtract` x)) xs

         solve n | x `mod` n == 0 = 0 
                 | otherwise      = n - (x `mod` n)

main :: IO ()
main = do (t:ts) <- fmap lines getContents
          foldM_ exec 1 (take (read t) ts)
  where exec i t = let (x:xs) = map read (words t)
                   in do putStr "Case #"
                         putStr (show i)
                         putStr ": "
                         putStrLn (show $ apocalypse (take (fromIntegral x) xs))
                         return (i+1)
