import Battery
import Window

import Control.Monad
import Control.Concurrent
import Data.Maybe

printStatus :: IO ()
printStatus = do
  (dpy, dftl, scr) <- windowInit
  time <- runBatt
  let compute = isJust time && not (isNaN . fromJust $ time)
      real_time = fromJust time
  if compute
      then putStrLn $ "Battery Time " ++ show (minutes $ floor real_time)
      else putStrLn $ "No Battery Time"
  if compute
      then createWarningWindow dpy dftl scr
      else return $ ()
  where minutes :: Int -> Int
        minutes x = x `div` 60

loop :: IO ()
loop = do
  printStatus
  threadDelay 6000000
  loop

main :: IO ()
main = loop
