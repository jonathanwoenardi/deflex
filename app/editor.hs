{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, TypeFamilies #-}

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Fix

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , PostBuild t m
           , MonadHold t m
           , MonadFix m
           )
        => m ()
headWidget = do
  elAttr "link" ("href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" 
              <> "rel" =: "stylesheet" 
              <> "type" =: "text/css") 
              blank

bodyWidget :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , PostBuild t m
           , MonadHold t m
           , MonadFix m
           )
        => m ()
bodyWidget = el "div" $ do
  elClass "table" "table" $ do
    el "thead" $ do
      el "tr" $ do
        elClass "td" "col-md-4" $ text "Key"
        elClass "td" "col-md-6" $ text "Value"
        elClass "td" "col-md-2" $ text "Action"
    el "tbody" $ do
      rowWidget "ABC" "DEF"
      rowWidget "GHI" "JKL"

rowWidget :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , PostBuild t m
           , MonadHold t m
           , MonadFix m
           )
          => T.Text -- ^ key
          -> T.Text -- ^ value
          -> m ()
rowWidget key val = el "tr" $ do
  rec
    elClass "td" "col-md-4" $ text key
    elClass "td" "col-md-6" $ do
      let hiddenInputD = attrsHidden <$> dynBool
          hiddenTextD  = attrsHidden . not <$> dynBool
      inputText <- elDynAttr "p" hiddenInputD $ textInput $ def & textInputConfig_initialValue .~ val
      elDynAttr "p" hiddenTextD $ dynText $ value inputText
      blank
    dynBool <- elClass "td" "col-md-2" $ do
      rec
        dynBool <- toggle True clickE
        clickE <- do
          (e, _) <- elClass' "button" "btn btn-primary" $ dynText (attrsAction <$> dynBool)
          return $ domEvent Click e
      return dynBool
  blank

attrsHidden :: Bool -> M.Map T.Text T.Text
attrsHidden True = "hidden" =: ""
attrsHidden False = M.empty

attrsAction :: Bool -> T.Text
attrsAction True = "Edit"
attrsAction False = "Save"