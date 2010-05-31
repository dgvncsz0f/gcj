import Data.List (splitAt)

data Viewpoint = Viewpoint { intersecs :: Int 
                           , wires     :: [(Int,Int)]
                           }

add :: (Int,Int) -> Viewpoint -> Viewpoint
add = add_ []
  where add_ acc (c,d) (Viewpoint k ((a,b):xs)) = if ((a<=c && b>=d) || c<=a && d>=b)
                                                  then add_ ((a,b):acc) (c,d) (Viewpoint (k+1) xs)
                                                  else add_ ((a,b):acc) (c,d) (Viewpoint k xs) 
        add_ acc wire (Viewpoint k [])          = Viewpoint k (wire:acc)

intersections :: [(Int,Int)] -> Int
intersections = intersecs . foldr add (Viewpoint 0 [])

main :: IO ()
main = do (t:ts) <- fmap lines getContents
          mapM_ printResult (zip [1..] . take (read t) . exec $ ts)
  where exec []      = []
        exec (x:xs0) = let (xs,ys) = splitAt (read x) xs0
                           points  = map ((\[a,b] -> (read a,read b)) . words) xs
                       in intersections points : exec ys

        printResult (k,r) = do putStr "Case #"
                               putStr (show k)
                               putStr ": "
                               putStrLn (show r)
