module Test.Main where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Test.EUnit (runTests, suite, test)
import Logger (debug, doLog, error, info)
  
main :: Effect Unit
main =
  void $ runTests do
    suite "Logger" do
      test "debug" do
        debug "This is a number" {misc: 42}
        debug "This is a string" {misc: "string"}
        debug "Empty" {}
      test "error" do
        error "This is a number" {misc: 42}
        error "This is a string" {misc: "string"}
        error "Empty" {}
      test "doLog" do
        doLog (atom "loggy" : atom "thing" : nil) info "Do-log" {int: 42, string: "hello"}

-- foreign import emergency :: forall a. String -> a -> Effect Unit
-- foreign import alert     :: forall a. String -> a -> Effect Unit
-- foreign import critical  :: forall a. String -> a -> Effect Unit
-- foreign import error     :: forall a. String -> a -> Effect Unit
-- foreign import warning   :: forall a. String -> a -> Effect Unit
-- foreign import notice    :: forall a. String -> a -> Effect Unit
-- foreign import info      :: forall a. String -> a -> Effect Unit
-- foreign import debug     :: forall a. String -> a -> Effect Unit