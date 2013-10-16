{-# OPTIONS -Wall -O2 #-}
-- Must compile with -threaded
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, filterM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.IORef
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)
import Data.Monoid (mconcat)
import Data.Time (getCurrentTime, FormatTime, formatTime, diffUTCTime)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(..))
import System.IO.Error (isEOFError)
import System.Locale (defaultTimeLocale)
import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8

showTime :: FormatTime t => t -> String
showTime time = fmt "%H:%M:%S." time ++ millis
  where
    fmt = formatTime defaultTimeLocale
    millis = take 3 $ fmt "%q" time

searchPath :: String -> IO [FilePath]
searchPath cmd = do
  paths <- nub . splitOn ":" <$> getEnv "PATH"
  return $ map (</> cmd) paths

toExec :: String -> IO FilePath
toExec cmd = do
  paths <- getPaths
  existingPaths <- filterM doesFileExist paths
  case existingPaths of
    [] -> fail $ "Cannot find " ++ show cmd
    [x] -> return x
    (x:xs) -> do
      hPutStrLn stderr $ "Warning: Using " ++ show x ++ " and not: " ++ show xs
      return x
  where
    getPaths
      | "/" `isInfixOf` cmd = return [cmd]
      | otherwise = searchPath cmd

timestamp :: IO String -> Handle -> Handle -> IO (IO ())
timestamp getTimeStr outH inH = do
  closedVar <- newEmptyMVar
  _ <- forkIO $ lineLoop `E.finally` putMVar closedVar ()
  return $ takeMVar closedVar
  where
    justEofs err
      | isEOFError err = Just ()
      | otherwise = Nothing
    catchEofs act f = E.catchJust justEofs act $ const f
    lineLoop = void . runMaybeT . forever $ do
      line <- MaybeT $
              (Just <$> BS8.hGetLine inH)
              `catchEofs` return Nothing
      time <- BS8.pack <$> lift getTimeStr
      lift . BS8.hPutStrLn outH $ BS8.unwords [time, line]

mkGetTimeStr :: Bool -> IO (IO String)
mkGetTimeStr delta = do
  curRef <- newIORef =<< getCurrentTime
  return $ do
    old <- readIORef curRef
    new <- getCurrentTime
    writeIORef curRef new
    return $
      if delta
      then '+' : show (new `diffUTCTime` old)
      else showTime new

unescape :: String -> String
unescape ('\\':'\\':xs) = '\\':unescape xs
unescape ('\\':'-':xs) = '-':unescape xs
unescape (x:xs) = x:unescape xs
unescape "" = ""

parseCmds :: [String] -> [[String]]
parseCmds = (map . map) unescape . splitOn ["--"]

parseCmdLine :: [String] -> (Bool, [[String]])
parseCmdLine ("delta" : args) = (True, parseCmds args)
parseCmdLine args = (False, parseCmds args)

runCmd :: IO String -> [String] -> IO ([IO ExitCode], [IO ()])
runCmd _ [] = fail "Empty cmdline is invalid"
runCmd getTimeStr (cmd:args) = do
  exec <- toExec cmd
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
  mapM_ (`hSetBuffering` LineBuffering) $ [ hOut, hErr ]
  pipeWaiters <-
    sequence
    [ timestamp getTimeStr stdout hOut
    , timestamp getTimeStr stderr hErr
    ]
  return
    ( [ waitForProcess procHandle ]
    , pipeWaiters
    )

-- "Usage: Timestamp [-delta] {cmdline...} [-- cmdline... [-- cmdline...]...]"
main :: IO ()
main = do
  (delta, cmds) <- parseCmdLine <$> getArgs
  getTimeStr <- mkGetTimeStr delta
  mapM_ (`hSetBuffering` LineBuffering) $ [ stdout, stderr ]
  (processWaiters, pipeWaiters) <- mconcat <$> mapM (runCmd getTimeStr) cmds
  exitCodes <- sequence processWaiters
  sequence_ pipeWaiters
  exitWith (foldr combineExitCodes ExitSuccess exitCodes)
  where
    -- Return the first error:
    combineExitCodes (ExitFailure x) _ = ExitFailure x
    combineExitCodes ExitSuccess other = other
