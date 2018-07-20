module Language.Lsl.Internal.Log(LogLevel(..), LogMessage(..),logLevelToName) where

import Language.Lsl.Internal.Util(LSLInteger)

data LogLevel = LogTrace | LogDebug | LogInfo | LogWarn | LogError
    deriving (Show,Eq,Ord)

data LogMessage = LogMessage { logMessageTime :: LSLInteger, logMessageLevel :: LogLevel,
                               logMessageSource :: String, logMessageText :: String }
    deriving (Show)

logLevelToName logLevel = case logLevel of
    LogTrace -> "TRACE"
    LogDebug -> "DEBUG"
    LogInfo -> "INFO"
    LogWarn -> "WARN"
    LogError -> "ERROR"
