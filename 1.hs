{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import Data.List.Split
import Text.Regex
import Data.Maybe
import Control.Monad
import Data.Conduit
import Control.Monad.IO.Class
import System.IO



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
	source "butterfly.txt" $$ conduit =$ sink