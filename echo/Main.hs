module Main where

import Maelstrom.Echo
import Node (node)

main :: IO ()
main = node echoHandler
