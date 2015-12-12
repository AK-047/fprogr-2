import Data.Matrix as M
import Data.Vector as V
import System.Random


main = do
    let testPercent = 0.80
    let test = []
    b <- rec test
    print b
    

isForTest :: Float -> IO Bool
isForTest percentage = do
    a <- (randomIO :: IO Float)
    return (a > percentage)


teach a test percentage = isForTest percentage >>= (\val -> if val then return (a:test) else return (test))
                                                                                             --teaching




rec test = do
    let nextRow = Prelude.length test --here goes reading 
    test <- teach nextRow test 0.5
    if (Prelude.length test < 5) then rec test else return test
