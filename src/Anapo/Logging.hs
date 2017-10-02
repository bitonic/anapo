{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Anapo.Logging
  ( logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Anapo.Text
import GHC.Stack
import Data.Monoid ((<>))

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe
  "console.log($1)"
  js_consoleLog :: Text -> IO ()

foreign import javascript unsafe
  "console.info($1)"
  js_consoleInfo :: Text -> IO ()

foreign import javascript unsafe
  "console.warn($1)"
  js_consoleWarn :: Text -> IO ()

foreign import javascript unsafe
  "console.error($1)"
  js_consoleError :: Text -> IO ()

{-# INLINE logError #-}
logError :: (HasCallStack, MonadIO m) => Text -> m ()
logError = liftIO . js_consoleError . addCallStack callStack

{-# INLINE logWarn #-}
logWarn :: (HasCallStack, MonadIO m) => Text -> m ()
logWarn = liftIO . js_consoleWarn . addCallStack callStack

{-# INLINE logInfo #-}
logInfo :: (HasCallStack, MonadIO m) => Text -> m ()
logInfo = liftIO . js_consoleInfo . addCallStack callStack

{-# INLINE logDebug #-}
logDebug :: (HasCallStack, MonadIO m) => Text -> m ()
logDebug = liftIO . js_consoleLog . addCallStack callStack

#else

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.IO as T

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO (newMVar ())

withLogLock :: IO a -> IO a
withLogLock m = withMVar logLock (\_ -> m)

logError :: (HasCallStack, MonadIO m) => Text -> m ()
logError x = liftIO (withLogLock (T.putStrLn (addCallStack callStack ("[ERROR] " <> x))))

logWarn :: (HasCallStack, MonadIO m) => Text -> m ()
logWarn x = liftIO (withLogLock (T.putStrLn (addCallStack callStack ("[WARN] " <> x))))

logInfo :: (HasCallStack, MonadIO m) => Text -> m ()
logInfo x = liftIO (withLogLock (T.putStrLn (addCallStack callStack ("[INFO] " <> x))))

logDebug :: (HasCallStack, MonadIO m) => Text -> m ()
logDebug x = liftIO (withLogLock (T.putStrLn (addCallStack callStack ("[DEBUG] " <> x))))

#endif

addCallStack :: CallStack -> Text -> Text
addCallStack stack = case getCallStack stack of
  [] -> id
  (_, SrcLoc{..}) : _ -> \txt -> "[" <> pack srcLocModule <> ":" <> pack (show srcLocStartLine) <> ":" <> pack (show srcLocStartCol) <> "] " <> txt

