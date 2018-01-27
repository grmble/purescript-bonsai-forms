module Bonsai.Forms.PureCss
  (alignedForm)
where

import Prelude

import Bonsai.Forms.Internal (FormDef, FormDefF(..), InputTyp(..), withAttributes)
import Bonsai.Forms.Model (FormModel, FormMsg(..), lookup, lookupChecked)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Control.Monad.Free (substFree)
import Data.Array as Array
import Data.CatList as CL
import Data.Foldable (for_, intercalate)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)


type NameStack =
  CL.CatList String

-- | Paint a PureCss aligned form from the form description.
-- |
-- | ID prefix can be provided to generate multiple forms with
-- | different IDs.
alignedForm :: Maybe String -> FormModel -> FormDef Unit -> H.Markup FormMsg Unit
alignedForm idPrefix model content =
  transform CL.empty content

  where
    transform :: forall a. NameStack -> FormDef a -> H.Markup FormMsg a
    transform ns c =
      substFree (transformF ns) c

    transformF :: forall a. NameStack -> FormDefF a -> H.Markup FormMsg a
    transformF _ (EmptyF x) =
      pure x

    transformF ns (FormF f x) = do
      let ns' = CL.snoc ns f.name
      let c = transform ns' f.content
      let n = intercalate "_" ns'

      -- the keyedElement guards against weirdness with
      -- input element reuse by the virtual dom
      H.keyedElement "form"
        (Array.fromFoldable
          ( f.attribs <>
            (CL.cons (A.cls "pure-form") $
             CL.cons (A.cls "pure-form-aligned") $
             CL.cons (E.onSubmit FormOK) $
             CL.empty)))
        [ Tuple n $ H.render $
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
                  H.! E.onClick FormCancel $
                  H.text "Cancel"
        ]
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

        -- everything else is a wrapper on text
        _ -> transformInput n id i *> pure x

    transformF ns (GroupedF g x) = do
      let n = intercalate "_" (CL.snoc ns g.name)

      H.div H.! A.cls "pure-controls" $ do
        transformGrouped n g.typ g.attribs g.inputs
        transformMessage g.message
      pure x

    transformInput n id i = do
      H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for id $ H.text i.label
        (H.input `withAttributes` i.attribs) H.!
          A.typ (show i.typ) H.!
          A.id id H.!
          E.onInput (FormSingle n) H.!
          A.value (fromMaybe "" (lookup n model))
        transformMessage i.message

    transformGrouped n typ props inputs = do
      for_ inputs \tup ->
        H.label H.! A.cls ("pure-" <> show typ) $ do
          (H.input `withAttributes` props) H.!
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
