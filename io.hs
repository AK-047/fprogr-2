{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import Data.List.Split
import Text.Regex
import Data.Maybe
import Control.Monad
import Data.Conduit
import Control.Monad.IO.Class
import System.IO

data Params = Params
	{separator 		:: String
	,ignore_first 	:: Bool
	,ignore_header	:: Bool
	,input 			:: FilePath
	,percent 		:: Float
	,output			:: FilePath
	}
	deriving (Data,Typeable,Show)

params = Params
	{separator 		= "," 					&= 				help "CSV line separator"
	,ignore_first 	= def 					&= name "F" &= 	help "Ignore first column"
	,ignore_header	= def 					&= name "H" &=	help "Ignore header"
	,input 			= "butterfly.txt" 		&= typFile  &= 	help "Path to the input CSV file"
	,output			= def			 		&= typFile  &= 	help "Path to the output file (optional)"
	,percent 		= 0.80					&= 				help "Part of input data that will be used for boosting the classifier (decimal percent value)"
	} &=
	help 	"Haskell Bayes Naive Classifier implementation" &=
	summary "Lab2 by Anton Novikau & Pavel Kalashnikau" &=
	details ["For more information please refer to readme.md file"]

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
		
conduit = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            yield x
            conduit

sink = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x -> do
            liftIO $ putStrLn $ x
            sink

main = do
	args <- cmdArgs params
	source (input args) $$ conduit =$ sink