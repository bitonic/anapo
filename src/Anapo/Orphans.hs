{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Anapo.Orphans () where

#if defined(ghcjs_HOST_OS)
#else
import Control.Monad.IO.Unlift
import Language.Javascript.JSaddle (JSM(..))
import Prelude

instance MonadUnliftIO JSM where
  askUnliftIO = JSM $ do
    u <- askUnliftIO
    return (UnliftIO (\(JSM m) -> unliftIO u m))
#endif
