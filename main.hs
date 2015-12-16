import qualified Data.Vector as V
import System.Random
import Data.Maybe
import Control.Monad
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Data.List.Split
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy


sumKey :: String
sumKey = "SumKey"
 
countKey :: String
countKey = "CountKey"
 
sumSqrKey :: String
sumSqrKey = "SumSqrKey"

dispersionKey :: String
dispersionKey = "DispersionKey"

main = do
    let testPercent = 0.80
    let test = []
    let dictionary = M.fromList []
    source "butterfly.txt" $$ conduit test dictionary =$ sink

updateClassAtrributes x m = 
    update
    where
        update = M.insertWithKey (insertFunc) className xToMap m  
        xToMap = map (\val -> (M.fromList [(sumKey, val), (countKey, 1), (sumSqrKey, val**2)])) value
        className = M.keys x !! 0
        value = x M.! className
        insertFunc k nV oV = zipWith (\ m v -> updateWithKey m v [sumKey, countKey, sumSqrKey]) oV value -- oV,nv = List of maps

updateWithKey a v [] = a
updateWithKey a v (x:xs) 
    | x == sumKey = updateWithKey (M.adjust (+v) x a) v xs
    | x == countKey = updateWithKey (M.adjust (+1) x a) v xs
    | x == sumSqrKey = updateWithKey (M.adjust (+v**2) x a) v xs


isForTest :: Float -> IO Bool
isForTest percentage = do
    a <- (randomIO :: IO Float)
    return (a > percentage)


teach a test percentage = isForTest percentage >>= (\val -> if val then return (((readX a):test,val)) else return ((test,val)))
                                                                                        --teaching
readX :: String -> M.Map String [Float]
readX x = M.singleton (last splitted) (readFloat $ init splitted)
    where
        splitted = splitOn "," x



readFloat :: [String] -> [Float]
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
            yield (dict,testVectors)
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
            --liftIO $ print $ M.showTreeWith (\k x -> show (k,x)) False True x
            let (dict,test) = x
            let (result, _) = runState calculateDispersion dict
            --let (a,b) = runState calculateProbability (result,"sasi")
            a <- liftIO $ singleProbability (result M.! "0")
            liftIO $ print $ result
            liftIO $ print $ a
            sink



calculateDispersion :: State (M.Map String [M.Map String Float]) (M.Map String [M.Map String Float])
calculateDispersion = do
    dictionary <- get
    return $ M.map (calculateDispersionList) dictionary
    where
        calculateDispersionList listMap = do
            map calculateDispersionInner listMap
        calculateDispersionInner innerMap = do
            let count = (innerMap M.! countKey)
            let sum = innerMap M.! sumKey
            let median = sum / count
            let sumSqr = innerMap M.! sumSqrKey
            M.insert dispersionKey ((1/(count - 1)) * (sumSqr + count*((median)^2) - 2*median*sum)) innerMap


calculateProbability :: State (M.Map String [M.Map String Float], [a]) Float
calculateProbability = do
	dict <- get
	return 0


singleProbability :: [M.Map String Float] -> IO Float
singleProbability m = do
	let sigma = m !! 0 M.! dispersionKey
	return sigma