module Main where

import Control.Monad.State

type Button = Int

type Time = Int

data BotHandle = A | B

data GameState = GameState { botA :: Bot
                           , botB :: Bot
                           , time :: Time
                           }

data Bot = Bot { button  :: Button
               , botTime :: Time
               }

ginit :: GameState
ginit = GameState (Bot 1 0) (Bot 1 0) 0

getBot :: BotHandle -> State GameState Bot
getBot A = get >>= return . botA
getBot B = get >>= return . botB

putBot :: (BotHandle,Bot) -> State GameState ()
putBot (A,bot) = get >>= \g -> put (g { botA = bot })
putBot (B,bot) = get >>= \g -> put (g { botB = bot })

timeToPush :: (BotHandle,Button) -> State GameState Time
timeToPush (bot,x) = do { nowTime <- getTime
                        ; y       <- fmap button (getBot bot)
                        ; myTime  <- fmap botTime (getBot bot)
                        ; let savings  = nowTime - myTime
                              moveTime = max 0 (abs (x - y) - savings)
                              pushTime = 1
                          in return (nowTime + pushTime + moveTime)
                        }

putTime :: Time -> State GameState ()
putTime t = get >>= \g -> put (g { time = t})

getTime :: State GameState Time
getTime = get >>= return . time

update :: (BotHandle,Button) -> State GameState ()
update (bot,x) = do { pushTime <- timeToPush (bot,x)
                    ; curTime  <- getTime
                    ; let nowTime = max pushTime curTime
                      in do { putBot (bot, Bot x pushTime)
                            ; putTime nowTime
                            }
                    }

parseBotInput :: [String] -> ([String], (BotHandle,Button))
parseBotInput (h:b:xs) = (xs, (read h, read b))

parseInput :: String -> [(BotHandle, Button)]
parseInput line = parseF (read n) ws id $ []
  where (n:ws) = words line
        parseF 0 _ acc  = acc
        parseF n ws acc = let (ws',e) = parseBotInput ws
                          in parseF (n-1) ws' (acc . (e:))

main :: IO ()
main = do { t <- fmap read getLine
          ; mapM_ (\k -> fmap parseInput getLine >>= \xs -> exec k xs) [1..t]
          }
  where exec :: Int -> [(BotHandle, Button)] -> IO ()
        exec k moves = let g = sequence_ (map update moves) >> getTime
                       in do { putStr "Case #"
                             ; putStr (show k)
                             ; putStr ": "
                             ; print (evalState g ginit)
                             }

instance Read BotHandle where
  readsPrec _ "O" = [(A,"")]
  readsPrec _ "B" = [(B,"")]
