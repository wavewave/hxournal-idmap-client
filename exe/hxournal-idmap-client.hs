module Main where

import System.Console.CmdArgs

import Database.HXournal.IDMap.Client.ProgType
import Database.HXournal.IDMap.Client.Command

main :: IO () 
main = do 
  putStrLn "hxournal-idmap-client"
  param <- cmdArgs mode
  commandLineProcess param