module Test.Main where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Test.EUnit (runTests, suite, test)
import Logger (debug, error, info, traceMetadata)
  
main :: Effect Unit
main =
  void $ runTests do
    suite "Logger" do
      test "debug" do
        debug (traceMetadata nil "This is a number") { misc: 42}
        debug (traceMetadata nil "This is a string") {misc: "string"}
        debug (traceMetadata nil "Empty") {}
      test "error" do
        error (traceMetadata nil "This is a number") { misc: 42}
        error (traceMetadata nil "This is a string") {misc: "string"}
        error (traceMetadata nil "Empty") {}

-- foreign import emergency :: forall a. String -> a -> Effect Unit
-- foreign import alert     :: forall a. String -> a -> Effect Unit
-- foreign import critical  :: forall a. String -> a -> Effect Unit
-- foreign import error     :: forall a. String -> a -> Effect Unit
-- foreign import warning   :: forall a. String -> a -> Effect Unit
-- foreign import notice    :: forall a. String -> a -> Effect Unit
-- foreign import info      :: forall a. String -> a -> Effect Unit
-- foreign import debug     :: forall a. String -> a -> Effect Unit
