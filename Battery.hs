module Battery (runBatt) where

import System.FilePath ((</>))
import Control.Exception (SomeException, handle)
import System.IO (IOMode(ReadMode), hGetLine, withFile)
import System.Posix.Files (fileExist)
import Data.Maybe
import Control.Monad.Reader

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

data Files = Files
  { fFull :: String
  , fNow :: String
  , fVoltage :: String
  , fCurrent :: String
  } | NoFiles

data Battery = Battery
  { full :: !Float
  , now :: !Float
  , power :: !Float
  } deriving (Show)

  
safeFileExist :: String -> String -> IO Bool
safeFileExist d f = handle noErrors $ fileExist (d </> f)
  where noErrors = const (return False) :: SomeException -> IO Bool

batteryFiles :: String -> IO Files
batteryFiles bat =
  do is_charge <- exists "charge_now"
     is_energy <- if is_charge then return False else exists "energy_now"
     is_current <- exists "current_now"
     plain <- if is_charge then exists "charge_full" else exists "energy_full"
     let cf = if is_current then "current_now" else "power_now"
         sf = if plain then "" else "_design"
     return $ case (is_charge, is_energy) of
       (True, _) -> files "charge" cf sf
       (_, True) -> files "energy" cf sf
       _ -> NoFiles
  where prefix = sysDir </> bat
        exists = safeFileExist prefix
        files ch cf sf = Files { fFull = prefix </> ch ++ "_full" ++ sf
                               , fNow = prefix </> ch ++ "_now"
                               , fCurrent = prefix </> cf
                               , fVoltage = prefix </> "voltage_now" }

haveAc :: FilePath -> IO Bool
haveAc f =
  handle onError $ withFile (sysDir </> f) ReadMode (fmap (== "1") . hGetLine)
  where onError = const (return False) :: SomeException -> IO Bool

readBattery :: Float -> Files -> IO Battery
readBattery _ NoFiles = return $ Battery 0 0 0
readBattery sc files =
    do a <- grab $ fFull files
       b <- grab $ fNow files
       d <- grab $ fCurrent files
       return $ Battery (3600 * a / sc) -- wattseconds
                        (3600 * b / sc) -- wattseconds
                        (d / sc) -- watts
    where grab f = handle onError $ withFile f ReadMode (fmap read . hGetLine)
          onError = const (return (-1)) :: SomeException -> IO Float

readBatteries :: [Files] -> IO (Maybe Float)
readBatteries bfs = do
  bats <- mapM (readBattery 1e6) (take 3 bfs)
  ac <- haveAc "AC/online"
  let watts = sum (map power bats)
      time' b = now b / watts
      time = sum $ map time' bats
  return $ if ac
             then Nothing
             else if isNaN time
                 then Nothing
                 else Just time

runBatt' :: [String] -> IO (Maybe Float)
runBatt' bfs = do
  liftIO $ readBatteries =<< mapM batteryFiles bfs

runBatt :: IO (Maybe Float)
runBatt = runBatt' ["BAT0","BAT1","BAT2"]

