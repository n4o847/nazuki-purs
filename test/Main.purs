module Test.Main where

import Prelude
import Effect (Effect)
import Test.Nazuki.CodeGen.Core (testGenerate)

main :: Effect Unit
main = do
  testGenerate
