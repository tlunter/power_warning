import Control.Monad.State
import Control.Concurrent
import System.Libnotify
import Data.Maybe
import Battery

data WarningLevel = VeryLowWarning | LowWarning | NoWarning
    deriving (Show, Eq, Ord)

data Vars = Vars {
    warned :: WarningLevel,
    time   :: Maybe Float
} deriving (Show)

defaultVars :: Vars
defaultVars = Vars { warned = NoWarning, time = Nothing }

notifyTime :: Int
notifyTime = 30

severeNotifyTime :: Int
severeNotifyTime = 10

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
  when ((isNothing maybeTime || currTime > notifyTime) && (currWarned < NoWarning)) $
      put (vars { warned = NoWarning })
  when (isJust maybeTime && currTime < notifyTime && currWarned > LowWarning) $ do
      io $ oneShot "Alert!" "Your battery life is getting low" "" Nothing
      put (vars { warned = LowWarning })
  when (isJust maybeTime && currTime < severeNotifyTime && currWarned > VeryLowWarning) $ do
      io $ oneShot "Alert!" "Your battery life is getting VERY low" "" Nothing
      put (vars { warned = VeryLowWarning })
  where minutes :: Int -> Int
        minutes x = x `div` 60

setTime :: Maybe Float -> StateT Vars IO ()
setTime t = do
  vars <- get
  put (vars { time = t })

io :: IO a -> StateT Vars IO a
io = liftIO
