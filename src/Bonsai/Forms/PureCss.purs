module Bonsai.Forms.PureCss
where

import Prelude

import Bonsai.Forms (FormDef, FormDefF(..), FormModel(..), FormMsg(..), InputTyp(..))
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.VirtualDom as VD
import Control.Monad.Free (substFree)
import Data.CatList (CatList, empty, snoc)
import Data.Foldable (intercalate, traverse_)
import Data.Map (lookup)
import Data.Maybe (Maybe, fromMaybe, maybe)


type NameStack =
  CatList String

alignedForm :: Maybe String -> FormModel -> FormDef Unit -> H.Markup FormMsg Unit
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
      let ns' = snoc ns f.name
      let c = transform ns' f.content
      H.form H.! A.cls "pure-form pure-form-aligned"
        H.! E.onSubmit (const FormOK) $ do
          H.fieldset $ do
            maybe c (insertLegend c) f.legend
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
      let ns' = snoc ns f.name
      let c = transform ns' f.content
      H.fieldset $ do
        maybe c (insertLegend c) f.legend
      pure x

    transformF ns (InputF i x) = do
      let n = intercalate "_" (snoc ns i.name)
      let id = prefix n
      case i.typ of
        IText ->
          transformText n id i
        ICheckbox ->
          transformCB n id i
      pure x

    transformText n id i =
      H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for id $ H.text i.label
        H.vnode $ VD.node "input"
          ( i.attribs <>
            [ A.typ "text"
            , A.id id
            , E.onInput (FormPut n)
            , E.onKeyEnterEscape (const FormOK) (const FormCancel)
            , A.value (fromMaybe "" (lookup n model))
            , A.typ "text" ] )
          [ ]
        traverse_
          (\s -> H.span H.! A.cls "pure-form-message-inline" $ H.text s)
          i.message

    transformCB n id i = do
      H.div H.! A.cls "pure-controls" $ do
        H.label H.! A.for id  H.! A.cls "pure-checkbox" $ do
          H.vnode $ VD.node "input"
            ( i.attribs <>
              [ A.typ "checkbox"
              , A.id id
              , E.onCheckedChange (FormPutB n)
              , A.checked (maybe false (const true) (lookup n model))
              ] )
            [ ]
          H.text (" " <> i.label)
        traverse_
          (\s -> H.span H.! A.cls "pure-form-message-inline" $ H.text s)
          i.message

    insertLegend c s =
      (H.legend $ H.text s) *> c

    prefix n =
      maybe n
        (\p -> intercalate "_" [ p, n ])
        idPrefix
