{-# LANGUAGE TypeApplications #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
module Reflex.Dom.Dropdown (
  PopupConfig(..),
  popup,
)
where

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
      text,
      (=:),
      DomSpace(RawElement), mainWidgetWithCss)
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Reflex.Dom.Attrs

import Reflex.Dom ( button, foldDyn, def)
import Reflex.Dom (ffor)
import Language.Javascript.JSaddle.Warp (run)

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
        ] ++ _popupConfig_extraExterior cfg

      attrsInterior :: [Attrs t m]
      attrsInterior =
        [ "class" ~: ffor (_popupConfig_visible cfg) (\isVisible -> if isVisible then "show" else "")
        , "class" ~: "popup-interior"
        , "style" ~:
        ffor (_popupConfig_visible cfg) (\isVisible -> 
            case (isVisible, _popupConfig_hiddenOrNone cfg) of
              (True, _) -> M.fromList [("visibility", "visible"), ("display", "interior-block")] <> _popupConfig_extraOnShow cfg
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
demo = run 1234 $ mainWidgetWithCss extraStyle demoWidget

demoWidget :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadJSM (Performable m)
     , JS.IsElement (RawElement (DomBuilderSpace m))
     ) => m ()
demoWidget = do
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
