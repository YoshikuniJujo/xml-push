import Network

import TestSimple

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 54492
	testSimple h
