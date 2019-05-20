{-# LANGUAGE OverloadedStrings #-}
module Anapo.Blaze (render) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef, readIORef)
import Control.Monad.IO.Unlift (UnliftIO(..))
import Data.List (foldl')
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import Data.Monoid ((<>))
import Control.Monad (foldM)
import Data.Foldable (toList)
import qualified Data.HashSet as HS

import Anapo.Component.Internal (DomEnv(..), ActionTraverse(..), ActionEnv(..), Node, _componentName, Dispatch(..), _componentPositions, registerComponent, _componentNode, DomM(..), _componentContext, newComponent)
import qualified Anapo.VDOM.Internal as V

render :: forall state. state -> Node () state -> IO B.Builder
render st node0 = do
  comp <- newComponent st (\() -> node0)
  liftIO (writeIORef (_componentContext comp) (Just ()))
  vdom <- unDomM
    (do
      node <- _componentNode comp ()
      patches <- registerComponent (_componentName comp) (_componentPositions comp) ()
      return (foldl' V.addNodeCallback node patches))
    (UnliftIO (\_ -> fail "Trying to run JSM action from Anapo.Blaze.run"))
    ActionEnv
      { aeRegisterThread = \_ -> fail "Trying to register a thread from Anapo.Blaze.run"
      , aeHandleException = \_ -> fail "Trying to handle an exception from Anapo.Blaze.run"
      , aeDispatch = Dispatch (\_ _ _ -> fail "Trying to dispatch from Anapo.Blaze.run")
      }
    ActionTraverse
      { atToComp = id
      , atToState = id
      , atToContext = id
      }
    DomEnv
      { domEnvReversePath = []
      , domEnvDirtyPath = False
      , domEnvComponentName = _componentName comp
      }
    (Just ())
    st
    ()
  renderNode vdom

renderNode :: V.Node -> IO B.Builder
renderNode node = renderNodeBody (V.nodeBody node)
  where
    renderNodeBody :: V.NodeBody -> IO B.Builder
    renderNodeBody = \case
      V.NBElement element -> do
        patches <- readIORef (V.elementPatches element)
        (empty, children) <- case V.elementChildren element of
          V.ChildrenRawHtml txt ->
            return (T.null txt, B.fromText txt)
          V.ChildrenNormal children -> do
            empty <- (== 0) <$> readIORef (V.normalChildrenSize children)
            nodes <- readIORef (V.normalChildrenNodes children)
            b <- foldM
              (\acc a -> do
                  w <- renderNode a
                  return $! acc <> w)
              mempty
              (toList nodes)
            return (empty, b)
          V.ChildrenKeyed children -> do
            empty <- (== 0) . HMS.size <$> readIORef (V.keyedChildrenNodes children)
            order <- readIORef (V.keyedChildrenOrder children)
            nodes <- readIORef (V.keyedChildrenNodes children)
            b <- foldM
              (\acc k -> do
                  w <- renderNode (nodes HMS.! k)
                  return $! acc <> w)
              mempty
              (toList order)
            return (empty, b)
        let tagBuilder = B.fromHtmlEscapedText (V.elementTag element)
        return $
          B.fromText "<" <> tagBuilder <> renderPatches patches <> B.fromText ">" <>
          (if empty then mempty else children <> B.fromText "</" <> tagBuilder <> B.fromText ">")
      V.NBText txt -> return (B.fromHtmlEscapedText txt)
      V.NBRaw{} -> return mempty

    renderPatches patches = let
      (attributes, style, classes) = foldl'
        (\(attrs0, style0, classes0) patch -> case patch of
            V.EPStyle k v ->
              (attrs0, HMS.insert k v style0, classes0)
            V.EPTextAttribute k v ->
              (HMS.insert k v attrs0, style0, classes0)
            V.EPBoolAttribute k True ->
              (HMS.insert k "" attrs0, style0, classes0)
            V.EPBoolAttribute k False ->
              (HMS.delete k attrs0, style0, classes0)
            V.EPRawAttribute{} ->
              (attrs0, style0, classes0)
            V.EPTextProperty k v ->
              (HMS.insert k v attrs0, style0, classes0)
            V.EPBoolProperty k True ->
              (HMS.insert k "" attrs0, style0, classes0)
            V.EPBoolProperty k False ->
              (HMS.delete k attrs0, style0, classes0)
            V.EPRawProperty{} ->
              (attrs0, style0, classes0)
            V.EPEvent{} ->
              (attrs0, style0, classes0)
            V.EPClass cls ->
              (attrs0, style0, HS.insert cls classes0))
        (mempty, mempty, mempty)
        patches
      attributes_b = foldMap
        (\(k, v) ->
          B.fromText " " <> B.fromHtmlEscapedText k <>
          (if v == "" then mempty else B.fromText "=\"" <> B.fromHtmlEscapedText v <> B.fromText "\""))
        (HMS.toList attributes)
      style_b = if HMS.size style > 0
        then
          B.fromText " style=\"" <>
          B.fromHtmlEscapedText (T.intercalate "; " (map (\(k, v) -> k <> ": " <> v) (HMS.toList style))) <>
          "\""
        else mempty
      classes_b = if HS.size classes > 0
        then B.fromText " class=\"" <> B.fromHtmlEscapedText (T.intercalate " " (HS.toList classes)) <> B.fromText "\""
        else mempty
      in attributes_b <> style_b <> classes_b

