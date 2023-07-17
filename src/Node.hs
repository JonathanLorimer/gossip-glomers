module Node where

import Data.ByteString qualified as BS
import Control.Monad

type NodeHandler = BS.ByteString -> IO ()

node :: NodeHandler -> IO ()
node handler = forever do
  ln <- BS.getLine
  print ln
  handler ln
