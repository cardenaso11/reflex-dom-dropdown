{-# LANGUAGE TypeApplications #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MultilineStrings #-}
module Reflex.Dom.Dropdown where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified GHCJS.DOM.Element as JS
import Language.Javascript.JSaddle
import Reflex.Dom.Core
    ( DomBuilder(DomBuilderSpace),
      MonadHold(),
      PostBuild(..),
      PerformEvent(Performable),
      TriggerEvent,
      Reflex(Dynamic),
      el,
      text,
      (=:),
      elDynAttr,
      simpleList,
      DomSpace(RawElement) )

--FIXME(Elaine): debug
import Reflex.Dom (mainWidget)
import Control.Monad (void)

-- | Information about the current scroll state of a lazy list
data ScrollInfo = ScrollInfo
  { _scrollInfo_topIndex :: Int
  -- ^ The index of the first visible row
  , _scrollInfo_numberOfElements :: Int
  -- ^ The number of rows needed to fill the viewport (with some buffer)
  , _scrollInfo_paddingTop :: Double
  -- ^ The pixel size of the padding needed above the first visible row to
  -- maintain the correct scrollbar size and scroll positioning.
  , _scrollInfo_paddingBottom :: Double
  -- ^ The pixel size of the padding needed above the first visible row to
  -- maintain the correct scrollbar size and scroll positioning.
  }
  deriving stock (Show)

--TODO(Elaine): Probably should reorganize and document these fields
data PopupConfig t state = PopupConfig
  { _popupConfig_visible :: Dynamic t Bool
  , _popupConfig_extraShow :: Text
  , _popupConfig_extraStyle :: Text
  , _popupConfig_hiddenOrNone :: Bool
  --TODO(Elaine): hange hiddenornone?
  , _popupConfig_extraInterior :: Text
  --TODO(Elaine): should extraInterior exist if we have extraStyle?
  , _popupConfig_extraExterior :: Text
  --TODO(Elaine): should extraExterior exist if we have extraStyle?
  , _popupConfig_identifier :: Text
  , _popupConfig_state :: Dynamic t [state]
  }

popup
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadJSM (Performable m)
     , JS.IsElement (RawElement (DomBuilderSpace m)), Eq state
     )
  => PopupConfig t state
  -> (Dynamic t state -> m a)
  -> m (Dynamic t [a])

popup cfg renderOne = do
  el "style" $ text $ mconcat
    [ "#", _popupConfig_identifier cfg, ".popup-exterior .popup-interior { ", if _popupConfig_hiddenOrNone cfg then "visibility: hidden;" else "display: none;", _popupConfig_extraInterior cfg <> " } "
    , "#", _popupConfig_identifier cfg, ".popup-exterior .show { visibility: visible; display: block; "
    , _popupConfig_extraShow cfg
    , " }"
    , "#", _popupConfig_identifier cfg, ".popup-exterior { ",  _popupConfig_extraExterior cfg, " } "
    , _popupConfig_extraStyle cfg
    ]
  let attrsExterior = pure $ "class" =: "popup-exterior" <> "id" =: _popupConfig_identifier cfg
      attrsInterior =
        do
          isVisible :: Bool <- _popupConfig_visible cfg
          pure $ "class" =: ("popup-interior" <> if isVisible then " show" else "")
  elDynAttr "div" attrsExterior $ do
    elDynAttr "div" attrsInterior $ do
      simpleList (_popupConfig_state cfg) renderOne


extraShow = "-webkit-animation: fadeIn 1s; animation: fadeIn 1s;"
extraStyle = """/* Add animation (fade in the popup) */
@-webkit-keyframes fadeIn {
  from {opacity: 0;}
  to {opacity: 1;}
}

@keyframes fadeIn {
  from {opacity: 0;}
  to {opacity:1 ;}
}"""
demo = mainWidget $ void
  ( popup
    PopupConfig
      { _popupConfig_visible = pure True
      , _popupConfig_extraShow = extraShow
      , _popupConfig_extraStyle = extraStyle
      , _popupConfig_hiddenOrNone = False
      , _popupConfig_extraInterior = ""
      , _popupConfig_extraExterior = "color: blue"
      , _popupConfig_identifier = "sample-identifier"
      , _popupConfig_state = pure [()]
      }
    (const $ do
      text "Text inside popup")
  )