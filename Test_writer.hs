import Control.Concurrent
import Control.Monad
import System.IO (stdout, hSetBuffering, BufferMode(..))

main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    putStrLn "Test"
    threadDelay 1000000
