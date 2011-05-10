module Main (main) where

import Data.Bits
import Control.Monad

dropMinimum :: (Ord a) => [a] -> [a]
dropMinimum (x_:xs_) = myDrop x_ xs_
  where myDrop _ []   = []
        myDrop y (x:xs) 
          | y < x     = x : myDrop y xs
          | otherwise = y : myDrop x xs

seanPiles :: [Int] -> Maybe Int
seanPiles xs
  | xorValue==0    = Just $ sum (dropMinimum xs)
  | otherwise      = Nothing
    where xorValue = foldr xor 0 xs

main :: IO ()
main = do { t <- fmap read getLine
          ; mapM_ (\k -> parseInput >>= \xs -> exec k xs) [1..t]
          }
  where parseInput = do { n  <- fmap read getLine
                        ; fmap (map read . take n . words) getLine
                        }
                     
        exec :: Int -> [Int] -> IO ()
        exec k xs = case (seanPiles xs)
                    of Nothing -> do { putStr "Case #"
                                     ; putStr (show k)
                                     ; putStr ": "
                                     ; putStrLn "NO"
                                     }
                       Just x  -> do { putStr "Case #"
                                     ; putStr (show k)
                                     ; putStr ": "
                                     ; putStrLn (show x)
                                     }
