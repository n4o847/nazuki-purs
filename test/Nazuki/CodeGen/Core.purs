module Test.Nazuki.CodeGen.Core where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Nazuki.CodeGen.Core as Core
import Test.Assert (assertEqual)

testCore :: Effect Unit
testCore = do
  log "Nazuki.CodeGen.Core"
  testGenerate

testGenerate :: Effect Unit
testGenerate = do
  log "- Test generate"
  assertEqual
    { actual:
        Core.generate do
          Core.bfInc
          Core.bfOpn
          Core.bfFwd
          Core.bfInc
          Core.bfCls
    , expected: "+[>+]"
    }
