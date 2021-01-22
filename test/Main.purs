module Test.Main where

import Prelude
import Effect (Effect)
import Test.Nazuki.CodeGen.Core (testCore)
import Test.Nazuki.CodeGen.Util (testUtil)

main :: Effect Unit
main = do
  testCore
  testUtil
