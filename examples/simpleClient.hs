import Network
import TestSimple

main :: IO ()
main = testSimple =<< connectTo "localhost" (PortNumber 54492)
