import qualified Data.Matrix as M
import qualified Data.Vector as V
import System.Random
import Data.Maybe
import Control.Monad
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Data.List.Split
import qualified Data.Map.Strict as Map



sumKey :: String
sumKey = "SumKey"
 
countKey :: String
countKey = "CountKey"
 
sumSqrKey :: String
sumSqrKey = "SumSqrKey"

main = do
    let testPercent = 0.80
    let test = []
    let dictionary = Map.fromList []
    source "1.txt" $$ conduit test dictionary =$ sink

updateClassAtrributes x m = 
    update
    where
        update = Map.insertWithKey (insertFunc) className xToMap m  
        xToMap = map (\val -> (Map.fromList [(sumKey, val), (countKey, 1), (sumSqrKey, val**2)])) value
        className = Map.keys x !! 0
        value = x Map.! className
        insertFunc k nV oV = zipWith (\ m v -> updateWithKey m v [sumKey, countKey, sumSqrKey]) oV value -- oV,nv = List of maps

updateWithKey a v [] = a
updateWithKey a v (x:xs) 
    | x == sumKey = updateWithKey (Map.adjust (+v) x a) v xs
    | x == countKey = updateWithKey (Map.adjust (+1) x a) v xs
    | x == sumSqrKey = updateWithKey (Map.adjust (+v**2) x a) v xs


isForTest :: Float -> IO Bool
isForTest percentage = do
    a <- (randomIO :: IO Float)
    return (a > percentage)


teach a test percentage = isForTest percentage >>= (\val -> if val then return (((readX a):test,val)) else return ((test,val)))
                                                                                        --teaching
readX :: String -> Map.Map String [Double]
readX x = Map.singleton (last splitted) (readFloat $ init splitted)
    where
        splitted = splitOn "," x



readFloat :: [String] -> [Double]
readFloat = map read

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

conduit testVectors dict = do
    mx <- await
    case mx of
        Nothing -> do
        	--testing
        	--testing
        	--testing
            yield dict
        Just x -> do
            (testVectors, wasAdded) <- liftIO $ teach x testVectors 0.5
            --liftIO $ print $ testVectors
            --yield x
            if wasAdded then conduit testVectors dict else conduit testVectors $ updateClassAtrributes (readX x) dict

sink = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            --liftIO $ print $ Map.showTreeWith (\k x -> show (k,x)) False True x
            liftIO $ print $ x
            sink