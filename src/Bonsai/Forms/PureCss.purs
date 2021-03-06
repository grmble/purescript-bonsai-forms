module Bonsai.Forms.PureCss
  (alignedForm)
where

import Prelude

import Bonsai (Cmd)
import Bonsai.EventHandlers (constHandler, on, onWithOptions, targetValueHandler)
import Bonsai.Forms.Internal (FormDef, FormDefF(..), InputTyp(..), Name)
import Bonsai.Forms.Model (FormModel, FormMsg(..), lookup, lookupChecked)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.Html.Internal (withAttributes)
import Bonsai.Html.Internal as HI
import Control.Monad.Free (Free, substFree)
import Data.CatList as CL
import Data.Foldable (for_, intercalate)
import Data.Function.Memoize (memoize2)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (fst, snd)
import Foreign (Foreign, F)


type NameStack =
  CL.CatList Name

-- | Paint a PureCss aligned form from the form description.
-- |
-- | ID prefix can be provided to generate multiple forms with
-- | different IDs.
alignedForm :: Maybe Name -> FormModel -> FormDef -> H.Markup FormMsg
alignedForm idPrefix model content =
  transform CL.empty content

  where
    transform :: forall a. NameStack -> Free FormDefF a -> Free (H.MarkupF FormMsg) a
    transform ns c =
      substFree (transformF ns) c

    transformF :: forall a. NameStack -> FormDefF a -> Free (H.MarkupF FormMsg) a
    transformF _ (EmptyF x) =
      pure x

    transformF _ (CustomMarkupF markup x) = do
      markup
      pure x

    transformF ns (FormF f x) = do
      let ns' = CL.snoc ns f.name
      let c = transform ns' f.content
      let n = intercalate "_" ns'

      -- the keyedElement guards against weirdness with
      -- input element reuse by the virtual dom
      (H.keyedElement "form" `withAttributes` f.attribs)
        H.! A.cls "pure-form pure-form-aligned"
        H.! onWithOptions E.preventDefaultStopPropagation "submit" constFormOK
        $ do
          H.keyed n $ H.render $
            H.fieldset $ do
              transformLegend f.legend
              c
              H.div H.! A.cls "pure-controls" $ do
                H.button
                  H.! A.typ "submit"
                  H.! A.cls "pure-button pure-button-primary" $
                  H.text "OK"
                H.button
                  H.! A.typ "button"
                  H.! A.cls "pure-button"
                  H.! (on "click" constFormCancel) $
                  H.text "Cancel"
      pure x

    transformF ns (FieldsetF f x) = do
      let ns' = CL.snoc ns f.name
      let c = transform ns' f.content
      H.fieldset $ do
        transformLegend f.legend
        c
      pure x

    transformF ns (InputF i x) = do
      let n = intercalate "_" (CL.snoc ns i.name)
      let id = prefix n


      case i.typ of
        -- radio and checkbox are handled by GroupedF
        IRadio -> pure x
        ICheckbox -> pure x
        -- file needs custom handling
        IFile -> pure x

        ITextarea -> transformTextarea n id i *> pure x

        -- everything else is a wrapper on text
        _ -> transformInput n id i *> pure x

    transformF ns (GroupedF g x) = do
      let n = intercalate "_" (CL.snoc ns g.name)

      H.div H.! A.cls "pure-controls" $ do
        transformGrouped n g.typ g.attribs g.inputs
        transformMessage g.message
      pure x

    transformF ns (CustomControlF control x) = do
      let n = intercalate "_" (CL.snoc ns control.name)
      let id = prefix n
      transformControl n id control.label control.message (control.markup n)
      pure x

    transformControl n id label message markup = do
      H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for id $ H.text label
        markup H.! A.id id
        transformMessage message

    transformInput n id i =
      transformControl n id i.label i.message $
        H.input `HI.withAttributes` i.attribs H.!
          A.typ (show i.typ) H.!
          (on "input" (memoize2 formSingleHandler id n)) H.!
          A.value (fromMaybe "" (lookup n model))

    transformTextarea n id i =
      transformControl n id i.label i.message $
        H.textarea `HI.withAttributes` i.attribs H.!
          (on "input" (memoize2 formSingleHandler id n)) H.!
          A.value (fromMaybe "" (lookup n model))

    transformGrouped n typ props inputs = do
      for_ inputs \tup ->
        H.label H.! A.cls ("pure-" <> show typ) $ do
          (H.input `HI.withAttributes` props) H.!
            A.typ (show typ) H.!
            A.name n H.!
            E.onCheckedChange (changeHandler tup) H.!
            A.checked (lookupChecked n (fst tup) model) H.!
            A.value (fst tup)
          H.text (" " <> snd tup)

      where
        changeHandler tup b =
          case typ of
            ICheckbox ->
              FormCheck n (fst tup) b
            _ ->
              FormSingle n (fst tup)

    transformMessage msg =
      for_ msg ((H.span H.! A.cls "pure-form-message-inline") <<< H.text)

    transformLegend ms =
      for_ ms (H.legend <<< H.text)

    prefix n =
      maybe n
        (\p -> intercalate "_" [ p, n ])
        idPrefix


constFormOK :: Foreign -> F (Cmd FormMsg)
constFormOK =
  constHandler FormOK

constFormCancel :: Foreign -> F (Cmd FormMsg)
constFormCancel =
  constHandler FormCancel

-- memoizable handler for text input events
-- the first parameter is the for id, since it must be unique on the page
-- there should be no collisions
formSingleHandler :: String -> String -> Foreign -> F (Cmd FormMsg)
formSingleHandler _ name =
  targetValueHandler (FormSingle name)
