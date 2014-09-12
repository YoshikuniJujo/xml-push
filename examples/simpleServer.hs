import Control.Monad
import Control.Concurrent
import Network

import TestSimple

main :: IO ()
main = forever . (void . forkIO . testSimple . fst3 =<<) . accept
	=<< listenOn (PortNumber 54492)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
