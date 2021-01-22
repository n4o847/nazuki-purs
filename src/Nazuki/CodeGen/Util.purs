module Nazuki.CodeGen.Util
  ( Ptr
  , mem
  , mems
  , raw
  , forward
  , enter
  , backward
  , exit
  , at
  , add
  , sub
  , getc
  , putc
  , while
  , set
  , ifElse
  , ifElseMut
  , incs
  , decs
  , puts
  ) where

import Prelude hiding (add, sub)
import Control.Monad.State (State)
import Data.Array ((!!))
import Data.Char (toCharCode)
import Data.Maybe as Maybe
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (for_, traverse_)
import Data.Unfoldable (replicateA)
import Nazuki.CodeGen.Core (Gen, Oper, bfBwd, bfCls, bfDec, bfFwd, bfGet, bfInc, bfNop, bfOpn, bfPut)

newtype Ptr
  = Ptr Int

mem :: Int -> Ptr
mem = Ptr

mems :: Partial => Array Int -> Int -> Ptr
mems s i = Ptr (Maybe.fromJust (s !! i))

fromChar :: Char -> Oper
fromChar = case _ of
  '+' -> bfInc
  '-' -> bfDec
  '>' -> bfFwd
  '<' -> bfBwd
  '[' -> bfOpn
  ']' -> bfCls
  ',' -> bfGet
  '.' -> bfPut
  _ -> bfNop

raw :: String -> Oper
raw s = traverse_ fromChar $ toCharArray s

forward :: Int -> Oper
forward a =
  if a >= 0 then
    void $ (replicateA a bfFwd :: State Gen (Array Unit))
  else
    void $ (replicateA (negate a) bfFwd :: State Gen (Array Unit))

enter :: Ptr -> Oper
enter (Ptr a) = forward a

backward :: Int -> Oper
backward a = forward (negate a)

exit :: Ptr -> Oper
exit (Ptr a) = backward a

at :: Ptr -> Oper -> Oper
at p oper = do
  enter p
  oper
  exit p

add :: Ptr -> Int -> Oper
add p x =
  at p do
    if x >= 0 then
      void $ (replicateA x bfInc :: State Gen (Array Unit))
    else
      void $ (replicateA (negate x) bfDec :: State Gen (Array Unit))

sub :: Ptr -> Int -> Oper
sub p x = add p (negate x)

getc :: Ptr -> Oper
getc p = at p bfGet

putc :: Ptr -> Oper
putc p = at p bfPut

while :: Ptr -> Oper -> Oper
while p block = do
  at p bfOpn
  block
  at p bfCls

set :: Ptr -> Int -> Oper
set p x = do
  while p do
    sub p 1
  add p x

ifElse ::
  { flg :: Ptr
  , tmp :: Ptr
  , ifTrue :: Oper
  , ifFalse :: Oper
  } ->
  Oper
ifElse { flg, tmp, ifTrue, ifFalse } = do
  while flg do
    ifTrue
    sub tmp 1
    sub flg 1
  add flg 1
  add tmp 1
  while tmp do
    sub tmp 1
    sub flg 1
    ifFalse

-- If the value of `flg` changes after
-- executing `cons` or `alt`, preserve it.
ifElseMut ::
  { flg :: Ptr
  , tmp :: Ptr
  , ifTrue :: Oper
  , ifFalse :: Oper
  } ->
  Oper
ifElseMut { flg, tmp, ifTrue, ifFalse } = do
  while flg do
    ifTrue
    sub tmp 1
    while flg do
      sub flg 1
      sub tmp 1
  add flg 1
  add tmp 2
  while tmp do
    sub tmp 1
    sub flg 1
    while tmp do
      sub tmp 1
      ifFalse

incs :: Ptr -> Oper
incs p =
  at p do
    raw "[>]+<[-<]>"

decs :: Ptr -> Oper
decs p =
  at p do
    raw "-[++>-]<[<]>"

puts :: Ptr -> String -> Oper
puts p s =
  for_ (toCharArray s) \c -> do
    add p (toCharCode c)
    putc p
    sub p (toCharCode c)
