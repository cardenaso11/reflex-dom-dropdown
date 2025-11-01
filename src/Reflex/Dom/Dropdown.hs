{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MultilineStrings #-}

module Reflex.Dom.Dropdown (
  PopupConfig(..),
  popup,
)
where

import Control.Monad.Fix(MonadFix)
import Control.Monad.IO.Class()
import Data.Text (Text)
import Reflex.Dom.Core
    ( DomBuilder,
      MonadHold,
      PostBuild(..),
      PerformEvent,
      TriggerEvent,
      Reflex(Dynamic)
    )
import qualified Data.Map as M
import Reflex.Dom.Attrs

import Reflex.Dom (ffor)


data PopupConfig t m = PopupConfig
  { _popupConfig_identifier :: Text
  -- Applied to exterior div
  , _popupConfig_visible :: Dynamic t Bool
  -- Immediately show or hide the popup
  , _popupConfig_hiddenOrNone :: Bool
  -- True = use visibility:hidden when not visible, False = use display:none when not visible
  , _popupConfig_extraInterior :: [Attrs t m]
  -- Convenience field that adds extra attributes to the interior content div, which has class popup-interior
  , _popupConfig_extraExterior :: [Attrs t m]
  -- Convenience field that adds extra attributes to the popup container div, which has class popup-exterior
  , _popupConfig_extraStyleOnShow :: M.Map Text Text
  -- Convenience field that adds extra styling when the popup is visible, which has class show.
  -- If you need fade out effects or a more in-depth lifecycle, use _popupConfig_extraInterior and _popupConfig_extraExterior
  }

popup
  :: forall t m a . ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
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
              (True, _) -> M.fromList [("visibility", "visible"), ("display", "interior-block")] <> _popupConfig_extraStyleOnShow cfg
              (False, True) -> M.singleton "visibility" "hidden"
              (False, False) -> M.singleton "display" "none"
            )
        ] ++ _popupConfig_extraInterior cfg
  elAttrs "div" attrsExterior $ do
    elAttrs "div" attrsInterior $ do
      widget