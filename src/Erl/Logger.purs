module Logger
       ( emergency
       , alert
       , critical
       , error
       , warning
       , notice
       , info
       , debug
       , spy
       , addLoggerContext
       , setPrimaryLevel
       , getPrimaryLevel
       , traceMetadata
       , commandMetadata
       , eventMetadata
       , genericMetadata
       , EventType(..)
       , LogType(..)
       , LogLevel(..)
       , MinimalMetadata
       , MinimalMetadataFields
       , BasicMetadataFields
       , class SpyWarning
       ) where

import Prelude

import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, singleton)
import Prim.Row as Row
import Prim.TypeError (class Warn, Text)
import Record.Builder as Builder

data LogLevel = Emergency
              | Alert
              | Critical
              | Error
              | Warning
              | Notice
              | Info
              | Debug

instance showLogLevel :: Show LogLevel where
  show Emergency = "emergency"
  show Alert = "alert"
  show Critical = "critical"
  show Error = "error"
  show Warning = "warning"
  show Notice = "notice"
  show Info = "info"
  show Debug = "debug"

data LogType = Trace
             | Event
             | Command
             | Audit

data EventType = Start
               | Stop

type MinimalMetadataFields a =
  ( domain :: List Atom
  , "type" :: LogType
  | a
  )

type BasicMetadataFields = MinimalMetadataFields ( text :: String)

type EventMetadataFields = MinimalMetadataFields ( event :: EventType
                                                 , text :: String)

type MinimalMetadata a = Record (MinimalMetadataFields a)

type BasicMetadata = Record BasicMetadataFields

type EventMetadata = Record EventMetadataFields

class SpyWarning
instance warn :: Warn (Text "Logger.spy usage") => SpyWarning

foreign import emergency :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import alert :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import critical :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import error :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import warning :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import notice :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import info :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import debug :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import spyImpl :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import addLoggerContext :: forall r. Record r -> Effect Unit
foreign import getPrimaryLevelImpl :: LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> Effect LogLevel
foreign import setPrimaryLevelImpl :: Atom -> Effect Unit

getPrimaryLevel :: Effect LogLevel
getPrimaryLevel = getPrimaryLevelImpl Emergency Alert Critical Error Warning Notice Info Debug

setPrimaryLevel :: LogLevel -> Effect Unit
setPrimaryLevel level = setPrimaryLevelImpl $ logLevelToErl level

spy :: forall a. SpyWarning => String -> a -> a
spy str a = unsafePerformEffect do
  spyImpl { domain: singleton $ atom "spy"
          , "type": Trace
          , text: str} {spydata: a}
  pure a

traceMetadata :: List Atom -> String -> BasicMetadata
traceMetadata domain msg = { domain
                           , text: msg
                           , "type": Trace }

commandMetadata :: List Atom -> String -> BasicMetadata
commandMetadata domain msg = { domain
                             , text: msg
                             , "type": Command }

eventMetadata :: List Atom -> EventType -> String -> EventMetadata
eventMetadata domain event msg = { domain
                                 , text: msg
                                 , "type": Event
                                 , event}

genericMetadata :: forall metadata.
                   Row.Lacks "domain" metadata =>
                   Row.Lacks "type" metadata =>
                   Row.Lacks "text" metadata =>
                   List Atom -> String -> Record metadata -> MinimalMetadata (text :: String | metadata)
genericMetadata domain msg metadata  =
  Builder.build (Builder.insert (SProxy :: SProxy "domain") domain >>>
                 Builder.insert (SProxy :: SProxy "type") Trace >>>
                 Builder.insert (SProxy :: SProxy "text") msg) metadata


logLevelToErl :: LogLevel -> Atom
logLevelToErl Emergency = atom "emergency"
logLevelToErl Alert = atom "alert"
logLevelToErl Critical = atom "critical"
logLevelToErl Error = atom "error"
logLevelToErl Warning = atom "warning"
logLevelToErl Notice = atom "notice"
logLevelToErl Info = atom "info"
logLevelToErl Debug = atom "debug"