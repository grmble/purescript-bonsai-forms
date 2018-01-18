module Bonsai.Forms.PureCss
where

import Prelude

import Bonsai.Forms (FormDef, FormDefF(..), FormDefT, FormModel(..), FormMsg(..), Input(..), fieldset, form, mkTextInput, textInput)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.Html.Internal as I
import Control.Monad.Free (Free, liftF, substFree, runFreeM)
import Control.Monad.Reader (ReaderT(..), ask, lift, local, runReaderT)
import Control.Monad.State (State, StateT(..), gets, modify)
import Data.CatList (CatList, empty, snoc)
import Data.Foldable (intercalate)
import Data.Identity (Identity(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)


type GeneratorData =
  CatList String

type Generator =
  ReaderT GeneratorData (H.Markup FormMsg)

alignedForm :: FormModel -> FormDef Unit -> H.Markup FormMsg Unit
alignedForm (FormModel model) content =
  transform content empty

  where
    transform :: forall a. FormDef a -> CatList String -> H.Markup FormMsg a
    transform c ns =
      runReaderT (runFreeM generate c) ns

    generate :: forall a. FormDefF a -> Generator a
    generate (EmptyF x) =
      pure x

    generate (FormF f x) = do
      ns <- ask
      let c = transform f.content ns
      lift $
        H.form H.! A.cls "pure-form pure-form-aligned"
          H.! E.onSubmit (const FormOK) $ do
          c
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

    generate (FieldsetF f x) = do
      ns <- ask
      let ns' = maybe ns (snoc ns) f.name
      let c = transform f.content ns'
      lift $ H.fieldset $ do
        maybe c (withLegend c) f.legend
      pure x

    generate (InputF (InputTextInput ti) x) = do
      ns <- ask
      let n = intercalate "_" (snoc ns ti.name)
      lift $ H.div H.! A.cls "pure-control-group" $ do
        H.label H.! A.for n $ H.text ti.label
        H.input H.! A.id n
          H.! E.onInput (\s -> FormModelSet n s)
          H.! E.onKeyEnterEscape (const FormOK) (const FormCancel)
          H.! A.value (fromMaybe "" (lookup n model))
          H.!? (A.placeholder <$> ti.placeholder)
          H.! A.required ti.required
          H.! A.typ "text"
      pure x

    withLegend c s =
      (H.legend $ H.text s) *> c
