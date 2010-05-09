module Main where

import Control.Monad (foldM_)

curtCircuit :: Int -> Int -> Bool
curtCircuit n k = k `mod` n2 == (n2-1)
  where n2 = 2^n

main :: IO ()
main = do (t:ts) <- fmap lines getContents
          foldM_ exec 1 (take (read t) ts)
  where exec i t = let [n,k] = map read . words $ t
                   in case (curtCircuit n k)
                      of True -> do putStr "Case #"
                                    putStr (show i)
                                    putStrLn ": ON"
                                    return (i+1)
                         False -> do putStr "Case #"
                                     putStr (show i)
                                     putStrLn ": OFF"
                                     return (i+1)
