module Main where

import Network.Api.Support.Demo

main :: IO ()
main = demo >>= print
