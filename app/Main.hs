module Main where

import Control.DeepSeq
import System.CPUTime
import AVLTree
-- import Data.Tree.AVL as A
    


main :: IO ()
main = do
    -- n <- readLn
    let list = [1..50000]

    let tree = buildAVLTree list

    putStrLn ((show.height) tree)
