import Control.Monad.State
import Control.Concurrent
import System.Libnotify
import Data.Maybe
import Battery

data Vars = Vars {
    warned :: Bool,
    time   :: Maybe Float
} deriving (Show)

defaultVars = Vars { warned = False, time = Nothing }

notifyTime :: Int
notifyTime = 300

main :: IO ()
main = runStateT process defaultVars >> return ()
--
-- layer an infinite list of uniques over the IO monad
--

process :: StateT Vars IO ()
process = do
  time <- io $ runBatt
  setTime time
  displayWarning
  io $ threadDelay 6000000
  process

displayWarning :: StateT Vars IO ()
displayWarning = do
  vars <- get
  let maybeTime = time vars
      currWarned = warned vars
      currTime = minutes $ floor (fromJust maybeTime)
  io $ putStrLn $ show vars
  when ((isNothing maybeTime || currTime > notifyTime) && currWarned == True) $ do
      io $ putStrLn "Changing to false"
      put (vars { warned = False })
  when (isJust maybeTime && currTime < notifyTime && currWarned == False) $ do
      io $ oneShot "Battery!" ("Time left: " ++ show currTime) "" Nothing
      put (vars { warned = True })
  where minutes :: Int -> Int
        minutes x = x `div` 60

setTime :: Maybe Float -> StateT Vars IO ()
setTime t = do
  vars <- get
  put (vars { time = t })

io :: IO a -> StateT Vars IO a
io = liftIO
