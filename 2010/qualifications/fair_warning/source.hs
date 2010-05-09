module Main where

import Data.List (foldl')
import Control.Monad (foldM_)

gcds :: (Integral n) => [n] -> n
gcds = foldl' fixed_gcd 0
  where fixed_gcd 0 0 = 0
        fixed_gcd a b = gcd a b

apocalypse :: [Integer] -> Integer
apocalypse xs = solve . gcds . diff $ xs
  where  diff (y:ys) = map (y-) ys ++ diff ys
         diff []     = []

         solve n = gcds $ map offset xs
           where offset x = n - (x `mod` n)

main :: IO ()
main = do (t:ts) <- fmap lines getContents
          foldM_ exec 1 (take (read t) ts)
  where exec i t = let (x:xs) = map read (words t)
                   in do putStr "Case #"
                         putStr (show i)
                         putStr ": "
                         putStrLn (show $ apocalypse (take (fromIntegral x) xs))
                         return (i+1)
