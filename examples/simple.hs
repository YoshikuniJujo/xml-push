import TestPusher
import FilePusher
import System.IO

main :: IO ()
main = testPusher
	(undefined :: SimplePusher Handle)
	Zero
	("xml/read.xml", "tmp/write.xml")
