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

    source "butterfly.txt" $$ conduit test =$ sink

    let dictionary = Map.fromList [("1", [(Map.fromList [(sumKey, 1.0), (countKey, 1.0), (sumSqrKey, 1.0)]), (Map.fromList [(sumKey, 1.0), (sumSqrKey, 1.0), (countKey, 1.0)])])]
    let x = readX "2,3,1"
    let g = updateClassAtrributes x dictionary
    print g
--    print dictionary    M.insertWith (++) "1" b dictionary

--x = fromList [("1",                                [1,                                                        2, 3])]
--m = fromList [("1",[(Map.fromList [(sumKey, 3.0), (countKey, 5.0), (sumSqrKey, 2.0)])])]         map for 2
--updateClassAtrributes :: Map.Map String [Double] -> a -> a
updateClassAtrributes x m = 
    update
    where
        update = Map.insertWithKey (insertFunc) className xToMap m  
        xToMap = map (\val -> (Map.fromList [(sumKey, val), (countKey, 1), (sumSqrKey, val**2)])) value
        className = Map.keys x !! 0
        value = x Map.! className
        insertFunc k nV oV = zipWith (\ m v -> Map.adjust (+v) sumKey m) oV value -- oV,nv = List of maps
        mapToMap map1 map2 = Map.mapWithKey (\key v -> Map.adjust (+v) key map1) map2 --map-map


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
            --liftIO $ print $ testVectors
            yield x
            conduit testVectors

sink = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            sink