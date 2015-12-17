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
import Options
import Text.Printf


sumKey :: String
sumKey = "SumKey"
 
countKey :: String
countKey = "CountKey"
 
sumSqrKey :: String
sumSqrKey = "SumSqrKey"

dispersionKey :: String
dispersionKey = "DispersionKey"


data MainOptions = MainOptions
    { optInput :: String
    , optDelimiter :: String
    ,optOut :: String
    ,optAttempts :: Int
    ,optPercent :: Double
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "input" "butterfly.txt"
            "CSV-file path."
        <*> simpleOption "delimiter" ","
            "CSV delimeter."
        <*> simpleOption "out" ""
            "Output File"    
        <*> simpleOption "attempts" 2
            "Attempts number"
        <*> simpleOption "percent" 80
            "Teaching percent"   
-- Command line options




main = runCommand $ \opts args -> do
    let fileName = (optInput opts)
    let delimiter = (optDelimiter opts)
    let percent = (optPercent opts)

    result <- source fileName $$ conduit [] (M.fromList []) percent =$ sink
    startAttempts (take ((optAttempts opts)-1) $ repeat 0) result fileName percent




startAttempts [] best file percent = do
    printResult (snd best)
startAttempts (x:xs) best file percent = do
    result <- source file $$ conduit [] (M.fromList []) percent =$ sink
    if (fst result) < (fst best) then startAttempts xs result file percent else startAttempts xs best file percent
    


printResult result = do
    putStr  $ M.foldlWithKey getClassWithAttrs "" result
    where
        getClassWithAttrs res k val = res ++ k ++":" ++ (snd (getAttrs val)) ++ "\n"
        getAttrs m = foldl composeAttrs (1,"") m
        composeAttrs (number,res) val = (number + 1, res ++ "-" ++ (show number) ++ "(" ++ (show (val M.! dispersionKey)) ++ "," ++ (show ((val M.! sumKey) / (val M.! countKey))) ++ ")")



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


isForTest :: Double -> IO Bool
isForTest percentage = do
    a <- (randomIO :: IO Double)
    return (a > percentage)


teach a test percentage = isForTest percentage >>= (\val -> if val then return (((readX a):test,val)) else return ((test,val)))
                                                                                        --teaching

readDouble :: [String] -> [Double]
readDouble = map read



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


conduit testVectors dict percent = do
    mx <- await
    case mx of
        Nothing -> do
            yield (dict,testVectors)
        Just x -> do
            (testVectors, wasAdded) <- liftIO $ teach x testVectors percent
            if wasAdded then conduit testVectors dict percent else conduit testVectors (updateClassAtrributes (readX x) dict) percent

sink = do
    mx <- await
    case mx of
        Just x -> do
            let (dict,test) = x
            let (result, _) = runState calculateDispersion dict
            let probabilitiesMap = M.map (singleProbability test) result
            let res = getClassBelongings probabilitiesMap test
            return (res,result)



calculateDispersion :: State (M.Map String [M.Map String Double]) (M.Map String [M.Map String Double])
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


singleProbability :: [M.Map String [Double]] -> [M.Map String Double] -> [Double]
singleProbability testVectors m = 
    map (getProb m) testVectors
    where
        getProb attrMap vectorMap = getInnerProb attrMap (getVector vectorMap)
        getVector vMap = snd (M.elemAt 0 vMap)
        getInnerProb attrMap vectorList = foldl (*) 1 $ zipWith (calcProb) attrMap vectorList
        calcProb singleAttrMap singleX = (1/((2*pi*(dispersion singleAttrMap)^2))**(1/2)) * (exp (- ((singleX - (average singleAttrMap))^2) / (2 * (dispersion singleAttrMap)^2)))
        dispersion x = x M.! dispersionKey
        average x = (x M.! sumKey) / (x M.! countKey)


getClassBelongings :: M.Map String [Double] -> [M.Map String [Double]] -> Double
getClassBelongings probabilitiesMap testVectors = 
    sum (getErrorsList) / (fromIntegral (length getErrorsList))
    where
        getErrorsList = zipWith (\label vectorMap -> if label == (fst (M.elemAt 0 vectorMap)) then 0 else 1) getClassLabels testVectors
        getClassLabels = M.foldlWithKey (findMax (snd initialPair) (fst initialPair)) [] probabilitiesMap
        findMax maxList maxKey memo key probabilitiesList = zipWith (\x y -> if x >= y then maxKey else key) maxList probabilitiesList
        initialPair = (M.elemAt 0 probabilitiesMap)



readX :: String -> M.Map String [Double]
readX x = M.singleton (last splitted) (readDouble $ init splitted)
    where
        splitted = splitOn "," x
