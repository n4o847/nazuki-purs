module Nazuki.CodeGen.Core
  ( BfCmd
  , Gen
  , Oper
  , bfNop
  , bfInc
  , bfDec
  , bfFwd
  , bfBwd
  , bfOpn
  , bfCls
  , bfGet
  , bfPut
  , generate
  ) where

import Prelude
import Control.Monad.State (State, execState, modify_)
import Data.List (List(..), reverse, toUnfoldable, (:))
import Data.String.CodeUnits (fromCharArray)

data BfCmd
  = Inc
  | Dec
  | Fwd
  | Bwd
  | Opn
  | Cls
  | Get
  | Put

type Gen
  = { cmds :: List BfCmd
    }

type Oper
  = State Gen Unit

modifyCmds :: (List BfCmd -> List BfCmd) -> Oper
modifyCmds f = modify_ \gen -> gen { cmds = f $ gen.cmds }

empty :: Gen
empty = { cmds: Nil }

bfNop :: Oper
bfNop = do
  pure unit

bfInc :: Oper
bfInc =
  modifyCmds case _ of
    Dec : xs -> xs
    xs -> Inc : xs

bfDec :: Oper
bfDec =
  modifyCmds case _ of
    Inc : xs -> xs
    xs -> Dec : xs

bfFwd :: Oper
bfFwd =
  modifyCmds case _ of
    Bwd : xs -> xs
    xs -> Fwd : xs

bfBwd :: Oper
bfBwd =
  modifyCmds case _ of
    Fwd : xs -> xs
    xs -> Bwd : xs

bfOpn :: Oper
bfOpn = do
  modifyCmds (Opn : _)

bfCls :: Oper
bfCls = do
  modifyCmds (Cls : _)

bfGet :: Oper
bfGet = do
  modifyCmds (Get : _)

bfPut :: Oper
bfPut = do
  modifyCmds (Put : _)

toChar :: BfCmd -> Char
toChar = case _ of
  Inc -> '+'
  Dec -> '-'
  Fwd -> '>'
  Bwd -> '<'
  Opn -> '['
  Cls -> ']'
  Get -> ','
  Put -> '.'

generate :: Oper -> String
generate oper = fromCharArray $ toUnfoldable $ map toChar $ reverse $ _.cmds $ execState oper empty
