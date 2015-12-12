import Data.Matrix as M
import Data.Vector as V
import System.Random
import Data.Maybe
import Control.Monad
import Data.Conduit
import Control.Monad.IO.Class
import System.IO




main = do
    let testPercent = 0.80
    let test = []
    source "butterfly.txt" $$ conduit test =$ sink
    

isForTest :: Float -> IO Bool
isForTest percentage = do
    a <- (randomIO :: IO Float)
    return (a > percentage)


teach a test percentage = isForTest percentage >>= (\val -> if val then return ((a:test,val)) else return ((test,val)))
                                                                                             --teaching
source path = do
    file <- liftIO $ openFile path ReadMode
    readToEnd file
    where
        readToEnd file = do
            eof <- liftIO $ hIsEOF file
            if eof
                then return ()
                else do
                    line <- liftIO $ hGetLine file
                    yield line
                    readToEnd file
        
conduit testVectors = do
    mx <- await
    case mx of
        Nothing -> do
        	--testing
        	--testing
        	--testing
        	return ()
        Just x -> do
            (testVectors, wasAdded) <- liftIO $ teach x testVectors 0.8
            liftIO $ print $ testVectors
            liftIO $ print $ wasAdded
            yield x
            conduit testVectors

sink = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            liftIO $ putStrLn $ x
            sink
