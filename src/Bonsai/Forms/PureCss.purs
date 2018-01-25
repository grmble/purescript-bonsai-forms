module Bonsai.Forms.PureCss
where

import Prelude

import Bonsai.Forms (FormDef, FormDefF(..), FormModel, FormMsg(FormCheck, FormSet, FormCancel, FormOK), InputTyp(..), Prop(..), combineClasses, lookup, lookupChecked, toProperty)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Internal as HI
import Bonsai.Html.Events as E
import Bonsai.VirtualDom as VD
import Control.Monad.Free (substFree)
import Data.CatList (CatList, empty, snoc)
import Data.Foldable (any, findMap, for_, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
        [ combineClasses ["pure-form", "pure-form-aligned"] f.props
        , HI.stringProperty "autocomplete" (autocompleteValue "off" f.props)
        , E.onSubmit FormOK
        ]
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
      let ns' = snoc ns f.name
      let c = transform ns' f.content
      H.fieldset $ do
        transformLegend f.legend
        c
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
        transformGrouped n g.typ g.props g.inputs
        transformMessage g.message
      pure x

    transformText n id i = do
      H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for id $ H.text i.label
        H.vnode $ VD.node "input"
          ( (map toProperty $ ensureAutocomplete "off" i.props) <>
            [ A.typ "text"
            , A.id id
            , E.onInput (FormSet n)
            , A.value (fromMaybe "" (lookup n model))
            , A.typ "text" ] )
          [ ]
        transformMessage i.message

    transformGrouped n typ props inputs = do
      for_ inputs \tup ->
        H.label H.! A.cls ("pure-" <> show typ) $ do
          H.vnode $ VD.node "input"
            ( (map toProperty props) <>
              [ A.typ (show typ)
              , A.name n
              , E.onCheckedChange (changeHandler tup)
              , A.checked (lookupChecked n (fst tup) model)
              , A.value (fst tup)
              ])
            [ ]
          H.text (" " <> snd tup)

      where
        changeHandler tup b =
          case typ of
            ICheckbox ->
              FormCheck n (fst tup) b
            _ ->
              FormSet n (fst tup)

    transformMessage msg =
      for_ msg ((H.span H.! A.cls "pure-form-message-inline") <<< H.text)

    transformLegend ms =
      for_ ms (H.legend <<< H.text)

    prefix n =
      maybe n
        (\p -> intercalate "_" [ p, n ])
        idPrefix

    autocompleteValue :: String -> Array Prop -> String
    autocompleteValue ac props =
      fromMaybe ac $ findMap maybeAuto props

    ensureAutocomplete :: String -> Array Prop -> Array Prop
    ensureAutocomplete ac props =
      if hasAutocomplete props
        then props
        else props <> [ Autocomplete ac ]

    hasAutocomplete :: Array Prop -> Boolean
    hasAutocomplete props =
      any isAuto props

    isAuto (Autocomplete _) = true
    isAuto _= false

    maybeAuto (Autocomplete ac) = Just ac
    maybeAuto _ = Nothing
