module Main (main) where

import Control.Monad
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

type Base = Char

type BaseL = [Base]

type Composition = M.Map (Base,Base) Base

type Opposition  = S.Set (Base,Base)

pair :: Base -> Base -> (Base,Base)
pair a b
  | a <= b    = (a,b)
  | otherwise = (b,a)

combine :: Composition -> BaseL -> Maybe BaseL
combine db (x:y:xs) = case (M.lookup (pair x y) db)
                      of Nothing -> Nothing
                         Just e  -> Just (e : xs)
combine _ xs        = Nothing

oppose :: Opposition -> BaseL -> Maybe BaseL
oppose db (x:xs) = let shouldClear = or (map (\y -> S.member (pair x y) db) xs)
                   in if (shouldClear)
                      then Just []
                      else Nothing
oppose _ xs      = Nothing

invoke :: Composition -> Opposition -> BaseL -> Base -> BaseL
invoke cdb odb base e = fromJust (cBase `mplus` oBase `mplus` Just newBase)
  where newBase = e : base
        cBase   = combine cdb newBase
        oBase   = oppose odb newBase

showBaseL :: BaseL -> String
showBaseL [] = "[]"
showBaseL xs = '[' : myShow (reverse xs) ']'
  where myShow (x:y:xs) suffix = x : ',' : ' ' : myShow (y:xs) suffix
        myShow (x:xs) suffix   = x : [suffix]

parseComposition :: [String] -> ([String], Composition)
parseComposition (x:xs) = (drop n xs, cdb)
  where n = read x
        cdb = M.fromList $ map entry (take n xs)
        entry [a,b,v] = (pair a b, v)

parseOpposition :: [String] -> ([String], Opposition)
parseOpposition (x:xs) = (drop n xs, odb)
  where n = read x
        odb = S.fromList $ map entry (take n xs)
        entry [a,b] = pair a b

parseBase :: [String] -> ([String], BaseL)
parseBase (x:xs) = (drop 2 xs, baseL)
  where n = read x
        baseL = take n (head xs)

main :: IO ()
main = do { t <- fmap read getLine
          ; mapM_ (\k -> getLine >>= \xs -> myPrint k (myExec xs)) [1..t]
          }
  where myExec xs = let input = words xs
                        (input',cdb)  = parseComposition input
                        (input'',odb) = parseOpposition input'
                        (_,baseL)     = parseBase input''
                    in foldl (invoke cdb odb) [] baseL
        
        myPrint n baseL = do { putStr "Case #"
                             ; putStr (show n)
                             ; putStr ": "
                             ; putStrLn (showBaseL baseL)
                             }

