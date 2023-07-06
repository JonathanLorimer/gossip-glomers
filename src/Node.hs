module Node where
import Data.ByteString
import Control.Monad

type NodeHandler = ByteString -> IO ()

node :: NodeHandler -> IO ()
node handler = forever do
  ln <- readLn
  handler ln
