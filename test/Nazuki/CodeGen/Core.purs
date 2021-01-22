module Test.Nazuki.CodeGen.Core where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Nazuki.CodeGen.Core as CodeGen
import Test.Assert (assertEqual)

testGenerate :: Effect Unit
testGenerate = do
  log "Test generate"
  assertEqual
    { actual:
        CodeGen.generate do
          CodeGen.bfInc
          CodeGen.bfOpn
          CodeGen.bfFwd
          CodeGen.bfInc
          CodeGen.bfCls
    , expected: "+[>+]"
    }
