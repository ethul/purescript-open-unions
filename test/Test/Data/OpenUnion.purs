module Test.Data.OpenUnion where

import Prelude (Unit(), unit)

import Control.Monad.Free (Free(), liftF, mapF)

import Data.OpenUnion

data Teletype1F a = Print1 String a

type Teletype1 a = Free Teletype1F a

print1 :: String -> Teletype1 Unit
print1 a = liftF (Print1 a unit)

data Teletype2F a = Print2 String a

type Teletype2 a = Free Teletype2F a

print2 :: String -> Teletype2 Unit
print2 a = liftF (Print2 a unit)

data Teletype3F a = Print3 String a

type Teletype3 a = Free Teletype3F a

print3 :: String -> Teletype3 Unit
print3 a = liftF (Print3 a unit)

type TF = OpenUnion Teletype1F (OpenUnion Teletype2F Teletype3F)

type T a = Free TF a

injF :: forall f g a. (Inject f g) => Free f a -> Free g a
injF = mapF inj

r :: T Unit
r = injF (print1 "1")
