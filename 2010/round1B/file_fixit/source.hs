
import Data.List (inits,intercalate)
import qualified Data.Set as S

type Index = S.Set String

splitPath :: String -> [String]
splitPath = drop 2 . map (intercalate "/") . inits . split id
  where split acc ('/':xs) = acc [] : split id xs
        split acc (x:xs)   = split (acc.(x:)) xs
        split acc []       = [acc []]

index :: [String] -> Index
index = foldr S.insert (S.singleton "/")

mkdir :: Index -> String -> (Index,Int)
mkdir index0 = mkdirs_ 0 index0 . splitPath
  where mkdirs_ acc index []     = (index,acc)
        mkdirs_ acc index (p:ps) = case (S.member p index)
                                   of True  -> mkdirs_ acc index ps
                                      False -> mkdirs_ (acc+1) (S.insert p index) ps

mkdirs :: Index -> [String] -> Int
mkdirs _ []          = 0
mkdirs index0 (p:ps) = let (index,acc) = mkdir index0 p
                       in acc + mkdirs index ps

main :: IO ()
main = do (t:ts) <- fmap lines getContents
          mapM_ printResult (zip [1..] . take (read t) . exec $ ts)
  where exec (x:xs) = let [n,m]      = map read (words x)
                          (exist,ys) = splitAt n xs
                          (new,zs)   = splitAt m ys
                          idx        = index exist
                      in mkdirs idx new : exec zs
        
        printResult (k,x) = do putStr "Case #"
                               putStr (show k)
                               putStr ": "
                               putStrLn (show x)
