{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Concurrent
import Network

import TestSimple

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 54492
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testSimple h
