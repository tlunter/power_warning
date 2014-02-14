import Control.Monad.State
import Control.Concurrent
import Data.Maybe
import Battery

data Vars = Vars {
    warned :: Bool,
    time   :: Maybe Float
} deriving (Show)

defaultVars :: Vars
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
  battTime <- io $ runBatt
  setTime battTime
  displayWarning
  io $ threadDelay 6000000
  process

displayWarning :: StateT Vars IO ()
displayWarning = do
  vars <- get
  let maybeTime = time vars
      currWarned = warned vars
      currTime = minutes $ floor (fromJust maybeTime)
  when ((isNothing maybeTime || currTime > notifyTime) && currWarned == True) $
      put (vars { warned = False })
  when (isJust maybeTime && currTime < notifyTime && currWarned == False) $
      put (vars { warned = True })
  where minutes :: Int -> Int
        minutes x = x `div` 60

setTime :: Maybe Float -> StateT Vars IO ()
setTime t = do
  vars <- get
  put (vars { time = t })

io :: IO a -> StateT Vars IO a
io = liftIO
