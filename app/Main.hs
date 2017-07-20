{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, TypeFamilies #-}

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Fix

-- main :: IO ()
-- main = mainWidgetWithHead headWidget bodyWidget

-- headWidget :: ( DomBuilder t m
--            , DomBuilderSpace m ~ GhcjsDomSpace
--            , PostBuild t m
--            , MonadHold t m
--            )
--         => m ()
-- headWidget = do
--   elAttr "link" ("href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" 
--               <> "rel" =: "stylesheet" 
--               <> "type" =: "text/css") 
--               blank
--   elAttr "script" ("src" =: "script.js") blank
--   return ()

-- bodyWidget :: ( DomBuilder t m
--            , DomBuilderSpace m ~ GhcjsDomSpace
--            , PostBuild t m
--            , MonadHold t m
--            )
--         => m ()
-- bodyWidget = do 
--   newTweet <- el "div" $ do
--     tweetBox <- textArea $
--       def & attributes .~ constDyn ("maxlength" =: "140")
--     tweetButton <- button "Tweet!"
--     displayNumChars tweetBox
--     return $ tag (current (value tweetBox)) tweetButton
--   el "div" $ do
--     latestTweet <- holdDyn "" newTweet
--     text "Last status: "
--     dynText latestTweet

-- displayNumChars :: (DomBuilder t m, PostBuild t m) => TextArea t -> m ()
-- displayNumChars textArea = do
--   let numChars = fmap T.length $ value textArea
--   display numChars
--   text " characters"





-- main :: IO ()
-- main = mainWidget body

-- body :: MonadWidget t m => m ()
-- body = do
--   el "h1" $ text "Write into TextInput Widget"
--   el "button" $ text "Click me"
--   rec t1 <- textInput $ def & setValue .~ evText3
--       evCopy1 <- button ">>>"
--       let evText1 = tagPromptlyDyn (value t1) evCopy1
--       t2 <- textInput $ def & setValue .~ evText1
--       evCopy2 <- button ">>>"
--       let evText2 = tagPromptlyDyn (value t2) evCopy2
--       t3 <- textInput $ def & setValue .~ evText2
--       evCopy3 <- button ">>>"
--       let evText3 = tagPromptlyDyn (value t3) evCopy3
--   return ()


-- main :: IO()
-- main = mainWidget bodyElement 

-- bodyElement :: MonadWidget t m => m ()
-- bodyElement = el "div" $ do
--   t1 <- textInput def & setValue
--   tb <- button "Edit"
--   let newTextEvent = tagPromptlyDyn (value t1) tb
--   str <- holdDyn "Click to edit me" newTextEvent
--   dynText str
--   dynText =<< holdDyn "ABC" newTextEvent
--   return ()



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



-- main :: IO()
-- main = mainWidget bodyWidget

-- bodyWidget :: MonadWidget t m => m ()
-- bodyWidget = el "div" $ do
--   el "table" $ do
--     el "tr" $ do
--       el "td" $ text "Key"
--       el "td" $ text "Value"
--       el "td" $ text "Action"
--     rowWidget "ABC" "DEF"
--   blank

-- rowWidget :: MonadWidget t m 
--           => T.Text -- ^ key
--           -> T.Text -- ^ value
--           -> m ()
-- rowWidget key val = el "tr" $ do
--   rec
--     el "td" $ text key
--     el "td" $ do
--       let hiddenInputD = attrsHidden <$> dynBool
--           hiddenTextD  = attrsHidden . not <$> dynBool
--       inputText <- elDynAttr "p" hiddenInputD $ textInput $ def
--       elDynAttr "p" hiddenTextD $ dynText $ value inputText
--       blank
--     dynBool <- el "td" $ do
--       rec
--         dynBool <- toggle True clickE
--         clickE <- do
--           (e, _) <- el' "button" $ dynText (attrsAction <$> dynBool)
--           return $ domEvent Click e
--       return dynBool
--   blank

-- attrsHidden :: Bool -> M.Map T.Text T.Text
-- attrsHidden True = "hidden" =: ""
-- attrsHidden False = M.empty

-- attrsAction :: Bool -> T.Text
-- attrsAction True = "Edit"
-- attrsAction False = "Save"