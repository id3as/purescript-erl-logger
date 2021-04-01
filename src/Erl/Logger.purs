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
       , setHandlerLevel
       , getHandlerLevel
       , traceMetadata
       , commandMetadata
       , eventMetadata
       , genericMetadata
       , LogType(..)
       , LogLevel(..)
       , MinimalMetadata
       , BasicMetadata
       , EventMetadata
       , CommandMetadata
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

derive instance eqLogLevel :: Eq LogLevel
instance ordLogLevel :: Ord LogLevel where
  compare lhs rhs = 
    let
      toInt :: LogLevel -> Int 
      toInt Emergency = 0
      toInt Alert = 1
      toInt Critical = 2
      toInt Error = 3
      toInt Warning = 4
      toInt Notice = 5
      toInt Info = 6
      toInt Debug = 7
    in
      compare (toInt lhs) (toInt rhs)

data LogType = Trace
             | Event
             | Command
             | Audit

type MinimalMetadata a =
  { domain :: List Atom
  , "type" :: LogType
  | a
  }

type BasicMetadata = MinimalMetadata ( text :: String )

type EventMetadata eventType = MinimalMetadata ( event :: eventType
                                               , text :: String)

type CommandMetadata commandType = MinimalMetadata ( command :: commandType
                                                   , text :: String)

type AuditMetadata auditType = MinimalMetadata ( audit :: auditType
                                               , text :: String)


traceMetadata :: List Atom -> String -> BasicMetadata
traceMetadata domain msg =
  genericMetadata domain Trace msg {}

commandMetadata :: forall commandType. List Atom -> commandType -> String -> CommandMetadata commandType
commandMetadata domain command msg =
  genericMetadata domain Command msg {command}

eventMetadata :: forall eventType. List Atom -> eventType -> String -> EventMetadata eventType
eventMetadata domain event msg =
  genericMetadata domain Event msg {event}

auditMetadata :: forall auditType. List Atom -> auditType -> String -> AuditMetadata auditType
auditMetadata domain audit msg =
  genericMetadata domain Audit msg {audit}

genericMetadata :: forall metadata.
                   Row.Lacks "domain" metadata =>
                   Row.Lacks "type" metadata =>
                   Row.Lacks "text" metadata =>
                   List Atom -> LogType -> String -> Record metadata -> MinimalMetadata (text :: String | metadata)
genericMetadata domain logType msg metadata  =
  Builder.build (Builder.insert (SProxy :: SProxy "domain") domain >>>
                 Builder.insert (SProxy :: SProxy "type") logType >>>
                 Builder.insert (SProxy :: SProxy "text") msg) metadata

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
foreign import getHandlerLevelImpl :: LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> LogLevel -> Atom -> Effect LogLevel
foreign import setHandlerLevelImpl :: Atom -> Atom -> Effect Unit

getPrimaryLevel :: Effect LogLevel
getPrimaryLevel = getPrimaryLevelImpl Emergency Alert Critical Error Warning Notice Info Debug

setPrimaryLevel :: LogLevel -> Effect Unit
setPrimaryLevel level = setPrimaryLevelImpl $ logLevelToErl level

getHandlerLevel :: Atom -> Effect LogLevel
getHandlerLevel = getHandlerLevelImpl Emergency Alert Critical Error Warning Notice Info Debug

setHandlerLevel :: LogLevel -> Atom -> Effect Unit
setHandlerLevel level handlerId  = setHandlerLevelImpl handlerId $ logLevelToErl level

spy :: forall a. SpyWarning => String -> a -> a
spy str a = unsafePerformEffect do
  spyImpl { domain: singleton $ atom "spy"
          , "type": Trace
          , text: str} {spydata: a}
  pure a

logLevelToErl :: LogLevel -> Atom
logLevelToErl Emergency = atom "emergency"
logLevelToErl Alert = atom "alert"
logLevelToErl Critical = atom "critical"
logLevelToErl Error = atom "error"
logLevelToErl Warning = atom "warning"
logLevelToErl Notice = atom "notice"
logLevelToErl Info = atom "info"
logLevelToErl Debug = atom "debug"
