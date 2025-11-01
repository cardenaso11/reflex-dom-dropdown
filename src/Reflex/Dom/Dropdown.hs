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
      MonadHold(holdDyn),
      PostBuild(..),
      PerformEvent(Performable),
      TriggerEvent,
      Reflex(Dynamic),
      el,
      text,
      (=:),
      elDynAttr,
      DomSpace(RawElement), mainWidget, mainWidgetWithCss)
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Reflex.Dom.Attrs

--FIXME(Elaine): debug
import Reflex.Dom ( ffor2, dynText, constDyn, button, foldDyn, def)
import Control.Monad (void)
import Reflex.Dom (ffor)
import Data.Map (Map)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Warp (run)

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

data PopupConfig t m = PopupConfig
  { _popupConfig_identifier :: Text
  , _popupConfig_visible :: Dynamic t Bool
  , _popupConfig_hiddenOrNone :: Bool
  , _popupConfig_extraInterior :: [Attrs t m]
  , _popupConfig_extraExterior :: [Attrs t m]
  , _popupConfig_extraOnShow :: M.Map Text Text
  }

popup
  :: forall t m a . ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadJSM (Performable m)
     , JS.IsElement (RawElement (DomBuilderSpace m))
     )
  => PopupConfig t m
  -> m a
  -> m a

popup cfg widget = do
  let
      attrsExterior :: [Attrs t m]
      attrsExterior =
        [ "class" ~: "popup-exterior"
        , "id" ~: _popupConfig_identifier cfg
        -- , "style" ~: mconcat ["visibility:visible;" , "display:block;"]
        ] ++ _popupConfig_extraExterior cfg

      -- thing1 :: m (Dynamic t Bool)
      -- thing1 = (pure $ _popupConfig_visible cfg)
      -- thing2 = (foldAttrs Nothing $ _popupConfig_extraOnShow cfg)
      attrsInterior :: [Attrs t m]
      attrsInterior =
        [ "class" ~: ffor (_popupConfig_visible cfg) (\isVisible -> if isVisible then "show" else "")
        , "class" ~: "popup-interior"
        , "style" ~:
        ffor (_popupConfig_visible cfg) (\isVisible -> 
            case (isVisible, _popupConfig_hiddenOrNone cfg) of
              (True, _) -> M.fromList [("visibility", "visible"), ("display", "block")] <> _popupConfig_extraOnShow cfg
              (False, True) -> M.singleton "visibility" "hidden"
              (False, False) -> M.singleton "display" "none" 
            )
        ] ++ _popupConfig_extraInterior cfg
  elAttrs "div" attrsExterior $ do
    elAttrs "div" attrsInterior $ do
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
demo = run 1234 $ mainWidgetWithCss extraStyle etc

etc :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadJSM (Performable m)
     , JS.IsElement (RawElement (DomBuilderSpace m))
     ) => m ()
etc = do
  text "The dropdown below lets you toggle the visibility of the popup."
  buttonToggleE <- button "Click to toggle popup"
  isVisibleD <- foldDyn (const not) False $ fmap (const True) buttonToggleE
  popup
      PopupConfig
        { _popupConfig_visible = isVisibleD
        , _popupConfig_hiddenOrNone = True
        , _popupConfig_extraInterior = pure $ mempty
        , _popupConfig_extraExterior = pure $ def {attrs_style=mconcat
            [ "color" =: "blue"]}
        , _popupConfig_extraOnShow = M.fromList
            [ ("-webkit-animation", "fadeIn 1s")
            , ("animation", "fadeIn 1s")
            ]
        , _popupConfig_identifier = "sample-identifier"
        }
      (do
        text "Text inside popup")
  text "This is some text that immediately follows the popup, later in the page"
  text "This is some more text"
