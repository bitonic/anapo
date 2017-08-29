{-# LANGUAGE CPP #-}
module Anapo.ClientM where

import qualified GHCJS.DOM.Types as DOM

-- How to run the client monad
-- --------------------------------------------------------------------

#if defined(ghcjs_HOST_OS)
runClientM :: ClientM () -> IO ()
runClientM = id
#else
import qualified Language.Javascript.JSaddle.Warp as DOM
runClientM :: ClientM () -> IO ()
runClientM cm = do
  putStrLn "Running on port 8000"
  DOM.run 8000 cm
#endif

type ClientM = DOM.JSM
