-- Must compile with -threaded
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (when, guard, forever, filterM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.IORef
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)
import Data.Time (getCurrentTime, formatTime, diffUTCTime)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv)
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(..))
import System.IO.Error (isEOFError)
import System.Locale (defaultTimeLocale)
import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import qualified Control.Exception as E
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

showTime time = fmt "%H:%M:%S." time ++ millis
  where
    fmt = formatTime defaultTimeLocale
    millis = take 3 $ fmt "%q" time

searchPath cmd = do
  paths <- nub . splitOn ":" <$> getEnv "PATH"
  filterM doesFileExist $ map (</> cmd) paths

toExec cmd
  | "/" `isInfixOf` cmd = return [cmd]
  | otherwise = searchPath cmd

timestamp getTimeStr outH inH = do
  closedVar <- newEmptyMVar
  forkIO $ lineLoop `E.finally` putMVar closedVar ()
  return $ takeMVar closedVar
  where
    justEofs err
      | isEOFError err = Just ()
      | otherwise = Nothing
    catchEofs act f = E.catchJust justEofs act $ const f
    lineLoop = void . runMaybeT . forever $ do
      line <- MaybeT $
              (Just <$> Text.IO.hGetLine inH)
              `catchEofs` return Nothing
      time <- Text.pack <$> lift getTimeStr
      lift . Text.IO.hPutStrLn outH $ Text.unwords [time, line]

mkGetTimeDelta = do
  curRef <- newIORef =<< getCurrentTime
  return $ do
    old <- readIORef curRef
    new <- getCurrentTime
    writeIORef curRef new
    return (new `diffUTCTime` old)

main = do
  rawArgs <- getArgs
  (delta, cmd, args) <- case rawArgs of
    "-delta" : cmd : args -> return (True, cmd, args)
    cmd : args -> return (False, cmd, args)
    _ -> fail "Usage: Timestamp [-delta] cmd [args...]"
  getTimeDelta <- mkGetTimeDelta
  let
    getTimeStr
      | delta = ('+' :) . show <$> getTimeDelta
      | otherwise = showTime <$> getCurrentTime
  execs <- toExec cmd
  exec <- case execs of
    [] -> fail $ "Cannot find " ++ show cmd ++ " in path"
    [x] -> return x
    (x:xs) -> do
      hPutStrLn stderr $ "Warning: Using " ++ show x ++ " and not: " ++ show xs
      return x
  (Nothing, Just hOut, Just hErr, procHandle) <- createProcess CreateProcess
    { cmdspec = RawCommand exec args 
    , cwd = Nothing
    , env = Nothing
    , std_in = Inherit
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = False
    , create_group = False
    }
  mapM_ (`hSetBuffering` LineBuffering) [ stdout, stderr, hOut, hErr ]
  closes <-
    sequence
    [ timestamp getTimeStr stdout hOut
    , timestamp getTimeStr stderr hErr
    ]
  exitCode <- waitForProcess procHandle
  sequence_ closes
  exitWith exitCode
