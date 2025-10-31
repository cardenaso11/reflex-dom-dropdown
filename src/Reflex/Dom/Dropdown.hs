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
      DomSpace(RawElement), SpiderHost )
import Data.ByteString (ByteString)
import qualified Data.Map as M

--FIXME(Elaine): debug
import Reflex.Dom (mainWidget, ffor2)
import Control.Monad (void)
import Reflex.Dom (ffor)
import Data.Map (Map)

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
data PopupConfig t = PopupConfig
  { _popupConfig_visible :: Dynamic t Bool
  -- , _popupConfig_extraShow :: Text
  -- , _popupConfig_extraStyle :: Text
  , _popupConfig_hiddenOrNone :: Bool
  --TODO(Elaine): hange hiddenornone?
  , _popupConfig_extraInterior :: Dynamic t (Map Text Text)
  --TODO(Elaine): should extraInterior exist if we have extraStyle?
  , _popupConfig_extraExterior :: Dynamic t (Map Text Text)
  --TODO(Elaine): should extraExterior exist if we have extraStyle?
  , _popupConfig_identifier :: Text
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
     , JS.IsElement (RawElement (DomBuilderSpace m))
     )
  => PopupConfig t
  -> m a
  -> m a

popup cfg widget = do
  -- el "style" $ text $ mconcat
  --   [ "#", _popupConfig_identifier cfg, ".popup-exterior .popup-interior { ", if _popupConfig_hiddenOrNone cfg then "visibility: hidden;" else "display: none;", _popupConfig_extraInterior cfg <> " } "
  --   , "#", _popupConfig_identifier cfg, ".popup-exterior .show { visibility: visible; display: block; "
  --   , _popupConfig_extraShow cfg
  --   , " }"
  --   , "#", _popupConfig_identifier cfg, ".popup-exterior { ",  _popupConfig_extraExterior cfg, " } "
  --   ]
  let attrsExterior = ffor (_popupConfig_extraExterior cfg) $ \extraExterior -> mconcat $
        [ "class" =: "popup-exterior"
        , "id" =: _popupConfig_identifier cfg
        , "visibility" =: "visible"
        , "display" =: "block"
        , extraExterior
        ]
      attrsInterior =
        do
          -- isVisible :: Bool <- _popupConfig_visible cfg
          ffor2 (_popupConfig_extraInterior cfg) (_popupConfig_visible cfg) $ \extraInterior isVisible -> mconcat $
            [ "class" =: ("popup-interior" <> if isVisible then " show" else "")
            , "style" =: (if _popupConfig_hiddenOrNone cfg
                then "visibility: hidden;"
                else "display: none; ") -- <> extraInterior
            -- , extraInterior
            ]
  elDynAttr "div" attrsExterior $ do
    elDynAttr "div" attrsInterior $ do
      widget


extraShow = "-webkit-animation: fadeIn 1s; animation: fadeIn 1s;"
extraStyle :: ByteString
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
      -- , _popupConfig_extraShow = extraShow
      -- , _popupConfig_extraStyle = extraStyle
      , _popupConfig_hiddenOrNone = False
      , _popupConfig_extraInterior = mempty
      , _popupConfig_extraExterior = pure $ "color" =: "blue"
      , _popupConfig_identifier = "sample-identifier"
      }
    (do
      text "Text inside popup")
  )