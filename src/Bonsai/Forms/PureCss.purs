module Bonsai.Forms.PureCss
where

import Prelude

import Bonsai.Forms (FormDef, FormDefF(..), FormModel, FormMsg(FormCheck, FormSet, FormCancel, FormOK), InputTyp(..), lookup, lookupChecked)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.VirtualDom as VD
import Control.Monad.Free (substFree)
import Data.CatList (CatList, empty, snoc)
import Data.Foldable (for_, intercalate)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)


type NameStack =
  CatList String

alignedForm :: Maybe String -> FormModel -> FormDef Unit -> H.Markup FormMsg Unit
alignedForm idPrefix model content =
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
      let n = intercalate "_" ns'

      -- the keyedElement guards against weirdness with
      -- input element reuse by the virtual dom
      H.keyedElement "form"
        [ A.cls "pure-form pure-form-aligned"
        , E.onSubmit FormOK
        ]
        [ Tuple n $ H.render $
            H.fieldset $ do
              maybe c (insertLegend c) f.legend
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
      let ns' = snoc ns f.name
      let c = transform ns' f.content
      H.fieldset $ do
        maybe c (insertLegend c) f.legend
      pure x

    transformF ns (InputF i x) = do
      let n = intercalate "_" (snoc ns i.name)
      let id = prefix n
      case i.typ of
        IText -> do
          transformText n id i
          pure x
        _ ->
          -- others handled by GroupedF
          pure x

    transformF ns (GroupedF g x) = do
      let n = intercalate "_" (snoc ns g.name)

      H.div H.! A.cls "pure-controls" $ do
        transformGrouped n g.typ g.inputs
        appendMessage g.message
      pure x

    transformText n id i = do
      H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for id $ H.text i.label
        H.vnode $ VD.node "input"
          ( i.attribs <>
            [ A.typ "text"
            , A.id id
            , E.onInput (FormSet n)
            , A.value (fromMaybe "" (lookup n model))
            , A.typ "text" ] )
          [ ]
        appendMessage i.message

    transformGrouped n typ inputs = do
      for_ inputs \tup ->
        H.label H.! A.cls ("pure-" <> show typ) $ do
          H.vnode $ VD.node "input"
            [ A.typ (show typ)
            , A.name n
            , E.onCheckedChange (changeHandler tup)
            , A.checked (lookupChecked n (fst tup) model)
            , A.value (fst tup)
            ]
            [ ]
          H.text (" " <> snd tup)

      where
        changeHandler tup b =
          case typ of
            ICheckbox ->
              FormCheck n (fst tup) b
            _ ->
              FormSet n (fst tup)

    appendMessage msg =
      for_ msg \s ->
        H.span H.! A.cls "pure-form-message-inline" $ H.text s

    insertLegend c s =
      (H.legend $ H.text s) *> c

    prefix n =
      maybe n
        (\p -> intercalate "_" [ p, n ])
        idPrefix
