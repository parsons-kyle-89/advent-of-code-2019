{-# language RankNTypes, FlexibleContexts #-}

import Data.Char
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Intcode ((==>))

main :: IO ()
main = do
  print (17 ==> 'a' ==> action)

action :: StateT Char (State Int) ()
action = do
  lift $ modify (-7+)
  modify toUpper
  return ()
