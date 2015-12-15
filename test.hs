import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy
import Data.Maybe
import Control.Monad
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List

sumKey :: String
sumKey = "SumKey"

countKey :: String
countKey = "CountKey"

sumSqrKey :: String
sumSqrKey = "SumSqrKey"

dispersionKey :: String
dispersionKey = "DispersionKey"

--                           Dictionary of key class attributes | Dictionary of dispersion for each class
calculateDispersion :: State (M.Map String [M.Map String Float]) (M.Map String [Float])
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
            (1/(count - 1)) * (sumSqr + count*((median)^2) - 2*median*sum)


updateOrAddByKey :: (M.Map String [M.Map String Float]) -> String -> String -> Float -> (M.Map String [M.Map String Float])
updateOrAddByKey dictionary globalKey localKey value = do
    let result = M.lookup globalKey dictionary
    case result of
        Just result -> M.update localUpdate globalKey dictionary
        Nothing -> M.insert globalKey [M.fromList [(localKey, value)]] dictionary
    M.fromList [("sasairises-1", [(M.fromList [("SumKey", 3.0), ("SumSqrKey", 5.0), ("CountKey", 2.0)]), (M.fromList [("SumKey", 6.0), ("SumSqrKey", 20.0), ("CountKey", 2.0)])])]
    where
        localUpdate list = do
            index <- List.findIndex emptyElementCondition list
            case index of
                Just index -> Fol.toList $ Seq.update index appendToMap $ Seq.fromList list
        emptyElementCondition innerMap = do
            result <- M.lookup innerMap localKey
            case result of
                Just result ->
                    return True
                Nothing ->
                    return False
        appendToMap innerMap = M.insert localKey value innerMap

   

--                   Dictionary of dispersion for each class, Vector | Dictionary of classes with probability
--calculateProbability :: State ((M.Map String [Maybe Float]), [Float]) (M.Map String [Float])
--calculateProbability = do
--    (dictionary, vector) <- get
--    return $ M.map (calculateProbabilityList vector) dictionary
--    where
--        calculateProbabilityList currentVector dispersionList = do
--            map (calculateProbabilityInner currentVector) dispersionList
--        calculateProbabilityInner concreteVector concreteDispersion = do
--            let floatDispersion = fromJust concreteDispersion




main = do
    let dictionary = M.fromList [("sasairises-1", [(M.fromList [("SumKey", 3.0), ("SumSqrKey", 5.0), ("CountKey", 2.0)]), (M.fromList [("SumKey", 6.0), ("SumSqrKey", 20.0), ("CountKey", 2.0)])])]
    let (result, dict) = runState calculateDispersion dictionary
    print result