module Bonsai.Forms.PureCss
where

import Prelude

import Bonsai.Forms (FormDef, FormDefF(..), FormModel(..), FormMsg(..), Input(..))
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Control.Monad.Free (substFree)
import Data.CatList (CatList, empty, snoc)
import Data.Foldable (intercalate)
import Data.Map (lookup)
import Data.Maybe (fromMaybe, maybe)
import Data.String (null)


type NameStack =
  CatList String

alignedForm :: String -> FormModel -> FormDef Unit -> H.Markup FormMsg Unit
alignedForm idPrefix (FormModel model) content =
  transform empty content

  where
    transform :: forall a. NameStack -> FormDef a -> H.Markup FormMsg a
    transform ns c =
      substFree (transformF ns) c

    transformF :: forall a. NameStack -> FormDefF a -> H.Markup FormMsg a
    transformF _ (EmptyF x) =
      pure x

    transformF ns (FormF f x) = do
      let ns' = maybe ns (snoc ns) f.name
      let c = transform ns' f.content
      H.form H.! A.cls "pure-form pure-form-aligned"
        H.! E.onSubmit (const FormOK) $ do
          H.fieldset $ do
            maybe c (withLegend c) f.legend
            H.div H.! A.cls "pure-controls" $ do
              H.button
                H.! A.typ "submit"
                H.! A.cls "pure-button pure-button-primary" $
                H.text "OK"
              H.button
                H.! A.cls "pure-button"
                H.! E.onClick FormCancel $
                H.text "Cancel"
      pure x

    transformF ns (FieldsetF f x) = do
      let ns' = maybe ns (snoc ns) f.name
      let c = transform ns' f.content
      H.fieldset $ do
        maybe c (withLegend c) f.legend
      pure x

    transformF ns (InputF (InputTextInput ti) x) = do
      let n = intercalate "_" (snoc ns ti.name)
      let id = withIdPrefix n
      H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for id $ H.text ti.label
        H.input H.! A.id id
          H.! E.onInput (FormPut n)
          H.! E.onKeyEnterEscape (const FormOK) (const FormCancel)
          H.! A.value (fromMaybe "" (lookup n model))
          H.!? (A.placeholder <$> ti.placeholder)
          H.! A.required ti.required
          H.!? (A.pattern <$> ti.pattern)
          H.! A.typ "text"
      pure x

    transformF ns (InputF (InputCheckboxInput ci) x) = do
      let n = intercalate "_" (snoc ns ci.name)
      let id = withIdPrefix n
      H.div H.! A.cls "pure-controls" $ do
        H.label H.! A.for id  H.! A.cls "pure-checkbox" $ do
          H.input H.! A.id id
            -- onChange ???
            H.! E.onCheckedInput (FormPutB n)
            H.! A.typ "checkbox"
            H.! A.checked false
          H.text ci.label
      pure x


    withLegend c s =
      (H.legend $ H.text s) *> c

    withIdPrefix n =
      if null idPrefix
        then n
        else intercalate "_" [ idPrefix, n ]
