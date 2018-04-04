{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- TODO replace all the @T.pack. show@ with tostring in js
module Main (main) where

import Anapo
import qualified Anapo.Text as T
import qualified GHCJS.DOM.Types as DOM
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Data.Monoid ((<>))
import Control.Lens (makeLenses, (.=), use, (%~))
import Control.Monad.Reader (ask)

#if defined(ghcjs_HOST_OS)
runJSM :: DOM.JSM () -> IO ()
runJSM = id
#else
import qualified Language.Javascript.JSaddle.Warp as Warp
runJSM :: DOM.JSM () -> IO ()
runJSM = Warp.run 8000
#endif

data Row = Row
  { _rowId :: {-# UNPACK #-} T.Text
  , _rowLabel :: {-# UNPACK #-} T.Text
  }
makeLenses ''Row

data State = State
  { _stateSelected :: Maybe T.Text
  , _stateRows :: MV.IOVector Row
  }
makeLenses ''State

adjectives :: V.Vector T.Text
adjectives = V.fromList ["pretty", "large", "big", "small", "tall", "short", "long", "handsome", "plain", "quaint", "clean", "elegant", "easy", "angry", "crazy", "helpful", "mushy", "odd", "unsightly", "adorable", "important", "inexpensive", "cheap", "expensive", "fancy"]

colours :: V.Vector T.Text
colours = V.fromList ["red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black", "orange"]

nouns :: V.Vector T.Text
nouns = V.fromList ["table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger", "pizza", "mouse", "keyboard"]

randomElement :: V.Vector a -> IO a
randomElement vec = do
  let len = V.length vec
  ix <- randomRIO (0, len-1)
  return (V.unsafeIndex vec ix)

buildData ::
     Int -- ^ how many to put in
  -> MV.IOVector Row
  -> IO (MV.IOVector Row)
buildData count0 rows0 = do
  let oldLen = MV.length rows0
  let newLen = oldLen + count0
  rows <- MV.grow rows0 count0
  let
    go !ix = if ix < newLen
      then do
        adj <- randomElement adjectives
        col <- randomElement colours
        noun <- randomElement nouns
        let label = adj <> " " <> col <> " " <> noun
        MV.unsafeWrite rows ix Row{_rowLabel = label, _rowId = T.pack (show ix)}
        go (ix+1)
      else return ()
  go oldLen
  return rows

updateData :: MV.IOVector Row -> IO ()
updateData rows = do
  let len = MV.length rows
  let
    go !ix = if ix < len
      then do
        row <- MV.unsafeRead rows ix
        MV.unsafeWrite rows ix (rowLabel %~ (<> " !!!") $ row)
        go (ix+10)
      else return ()
  go 0

rowsComponent :: Node a State
rowsComponent =
  div_ [class_ "container"] $ do
    n$ div_ [class_ "jumbotron"] $ do
      n$ div_ [class_ "row"] $ do
        n$ div_ [class_ "col-md-6"] $ do
          n$ h1_ [] (n$ "React keyed")
        n$ div_ [class_ "col-md-6"] $ do
          n$ div_ [class_ "row"] $ do
            n$ div_ [class_ "col-sm-6 smallpad"] $
              n$ button_ [type_ "button", class_ "btn btn-primary btn-block", id_ "run", onclick_ run] (n$ "Create 1,000 rows")
            n$ div_ [class_ "col-sm-6 smallpad"] $
              n$ button_ [type_ "button", class_ "btn btn-primary btn-block", id_ "runlots", onclick_ runLots] (n$ "Create 10,000 rows")
            n$ div_ [class_ "col-sm-6 smallpad"] $
              n$ button_ [type_ "button", class_ "btn btn-primary btn-block", id_ "add", onclick_ add] (n$ "Append 1,000 rows")
            n$ div_ [class_ "col-sm-6 smallpad"] $
              n$ button_ [type_ "button", class_ "btn btn-primary btn-block", id_ "update", onclick_ update] (n$ "Update every 10th row")
            n$ div_ [class_ "col-sm-6 smallpad"] $
              n$ button_ [type_ "button", class_ "btn btn-primary btn-block", id_ "clear", onclick_ clear] (n$ "Clear")
            n$ div_ [class_ "col-sm-6 smallpad"] $
              n$ button_ [type_ "button", class_ "btn btn-primary btn-block", id_ "swaprows", onclick_ swapRows] (n$ "Swap Rows")
    n$ table_ [class_ "table table-hover table-striped test-data"] $
      n$ tbody_ [] rowsDom
    n$ span_ [class_ "preloadicon glyphicon glyphicon-remove", attribute "aria-hidden" "true"] ()
  where
    runCount count _ ev = do
      preventDefault ev
      dispatch $ do
        rows <- liftIO (MV.new 0)
        rows' <- liftIO (buildData count rows)
        stateRows .= rows'

    run = runCount 1000
    runLots = runCount 10000

    add _ ev = do
      preventDefault ev
      dispatch $ do
        rows <- use stateRows
        rows' <- liftIO (buildData 1000 rows)
        stateRows .= rows'

    update _ ev = do
      preventDefault ev
      dispatch $ do
        rows <- use stateRows
        liftIO (updateData rows)

    clear _ ev = do
      preventDefault ev
      dispatch $ do
        rows <- liftIO (MV.new 0)
        stateRows .= rows

    swapRows _ ev = do
      preventDefault ev
      dispatch $ do
        rows <- use stateRows
        if MV.length rows > 998
          then liftIO (MV.swap rows 1 998)
          else return ()

    rowsDom :: KeyedDom a State
    rowsDom = do
      State{..} <- ask
      let len = MV.length _stateRows
      let
        go !ix = if ix < len
          then do
            Row{..} <- liftIO (MV.unsafeRead _stateRows ix)
            let cl = if Just _rowId == _stateSelected
                  then "danger"
                  else ""
            key _rowId $
              tr_ [class_ cl] $ do
                n$ td_ [class_ "col-md-1"] (n$ text _rowId)
                n$ td_ [class_ "col-md-4"] $ do
                  n$ a_ [onclick_ (select _rowId)] (n$ text _rowLabel)
                n$ td_ [class_ "col-md-1"] $ do
                  n$ a_ [onclick_ (delete ix)] $
                    n$ span_ [class_ "glyphicon glyphicon-remove", attribute "aria-hidden" "true"] ()
                n$ td_ [class_ "col-md-6"] ()
            go (ix+1)
          else return ()
      go 0

    select rid _ ev = do
      preventDefault ev
      dispatch (stateSelected .= Just rid)

    delete ix _ ev = do
      preventDefault ev
      dispatch $ do
        rows <- liftIO . V.unsafeFreeze =<< use stateRows
        let rows1 = V.slice 0 ix rows
        let rows2 = V.drop (ix+1) rows
        rows' <- liftIO (V.thaw (rows1 <> rows2))
        stateRows .= rows'

mainJSM :: DOM.JSM ()
mainJSM = installNodeBody
  (\cont -> do
      rows <- liftIO (MV.new 0)
      DOM.liftJSM (cont State{_stateRows = rows, _stateSelected = Nothing}))
  rowsComponent
  (\err -> text ("Got error: " <> T.pack (show err)))

main :: IO ()
main = do
  runJSM mainJSM
