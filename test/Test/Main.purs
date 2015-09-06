module Test.Main where

import Prelude (Unit(), pure, unit)

import Control.Monad.Eff (Eff())

import Test.Data.OpenUnion ()

main :: forall eff. Eff eff Unit
main = pure unit
