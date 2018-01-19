module Bonsai.Forms
  ( Fieldset
  , TextInput
  , CheckboxInput
  , Input(..)
  , FormDef
  , FormDefF(..)
  , FormDefT
  , FormMsg(..)
  , FormModel(..)
  , checkboxInput
  , mkTextInput
  , form
  , fieldset
  , textInput
  , emptyFormModel
  , updateForm
  )
where

import Prelude

import Bonsai (UpdateResult, plainResult)
import Control.Monad.Free (Free, liftF)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, delete, empty, insert)
import Data.Maybe (Maybe(..))


--
--
-- dsl data structures
--
--

type Fieldset =
  { name :: Maybe String
  , legend :: Maybe String
  , content :: FormDefT
  }

type TextInput =
  { name :: String
  , label :: String
  , required :: Boolean
  , pattern :: Maybe String
  , title :: Maybe String
  , placeholder :: Maybe String
  }

type CheckboxInput =
  { name :: String
  , label :: String
  }

data Input
  = InputTextInput TextInput
  | InputCheckboxInput CheckboxInput

data FormDefF a
  = FormF Fieldset a
  | FieldsetF Fieldset a
  | InputF Input a
  | EmptyF a

instance functorFormDefF :: Functor FormDefF where
  map f (FormF s x) = FormF s (f x)
  map f (FieldsetF rec x) = FieldsetF rec (f x)
  map f (InputF v x) = InputF v (f x)
  map f (EmptyF x) = EmptyF (f x)

type FormDef = Free FormDefF
type FormDefT = FormDef Unit

--
--
-- Form DSL
--
--

mkTextInput
  :: String
  -> String
  -> TextInput
mkTextInput name label =
  { name: name
  , label: label
  , required: false
  , pattern: Nothing
  , title: Nothing
  , placeholder: Nothing
  }

form :: Maybe String -> Maybe String -> FormDefT -> FormDefT
form name legend content =
  liftF $ FormF { name, legend, content } unit

fieldset :: Maybe String -> Maybe String -> FormDefT -> FormDefT
fieldset name legend content =
    liftF $ FieldsetF { name, legend, content } unit

textInput :: TextInput -> FormDefT
textInput ti =
  liftF $ InputF (InputTextInput ti) unit

checkboxInput :: String -> String -> FormDefT
checkboxInput name label =
  liftF $ InputF (InputCheckboxInput { name, label }) unit

--
--
-- form messages
--
--
data FormMsg
  = FormPut String String
  | FormPutB String Boolean
  | FormOK
  | FormCancel

newtype FormModel =
  FormModel (Map String String)

derive instance genericFormModel :: Generic FormModel _
instance showFormModel :: Show FormModel where show = genericShow

emptyFormModel :: FormModel
emptyFormModel =
  FormModel empty

updateForm :: forall eff. FormModel -> FormMsg -> UpdateResult eff FormModel FormMsg
updateForm (FormModel model) msg =
  plainResult $ FormModel $
    case msg of
      FormPut k v ->
         insert k v model
      FormPutB k b ->
        if b
          then insert k "on" model
          else delete k model
      _ ->
        model
