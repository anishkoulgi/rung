module Main (main) where

import Lib
import UI.GameMode

main :: IO ()
main = do
    mode <-  selectionMain
    print ()
