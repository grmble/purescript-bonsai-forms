module Bonsai.Forms
  ( Form
  , Fieldset
  , TextInput
  , Input(..)
  , FormDef
  , FormDefF(..)
  , FormDefT
  , FormMsg(..)
  , FormModel(..)
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
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))


--
--
-- dsl data structures
--
--

type Form =
  { content :: FormDefT
  }

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

data Input
  = InputTextInput TextInput

data FormDefF a
  = FormF Form a
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

form :: FormDefT -> FormDefT
form content =
  liftF $ FormF { content } unit

textInput :: TextInput -> FormDefT
textInput ti =
  liftF $ InputF (InputTextInput ti) unit

fieldset :: Maybe String -> Maybe String -> FormDefT -> FormDefT
fieldset name legend content =
    liftF $ FieldsetF { name, legend, content } unit


--
--
-- form messages
--
--
data FormMsg
  = FormModelSet String String
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
      FormModelSet k v ->
         insert k v model
      _ ->
        model
