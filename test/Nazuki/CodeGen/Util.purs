module Test.Nazuki.CodeGen.Util where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Nazuki.CodeGen.Core as Core
import Nazuki.CodeGen.Util as Util
import Test.Assert (assertEqual)

testUtil :: Effect Unit
testUtil = do
  log "Nazuki.CodeGen.Util"
  testAdd

testAdd :: Effect Unit
testAdd = do
  log "- Test add"
  assertEqual
    { actual:
        Core.generate do
          let
            a = Util.mem 0
          Util.add a 10
    , expected: "++++++++++"
    }
