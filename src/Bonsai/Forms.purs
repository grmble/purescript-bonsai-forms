module Bonsai.Forms
  ( Fieldset
  , Input
  , InputTyp(..)
  , FormDef
  , FormDefF(..)
  , FormDefT
  , FormMsg(..)
  , FormModel(..)
  , form
  , fieldset
  , textInput
  , checkboxInput
  , emptyFormModel
  , updateForm
  )
where

import Prelude

import Bonsai (UpdateResult, plainResult)
import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, liftF)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, delete, empty, insert)
import Data.Maybe (Maybe)


--
--
-- dsl data structures
--
--

type Fieldset =
  { name :: String
  , legend :: Maybe String
  , content :: FormDefT
  }

type Input =
  { typ :: InputTyp
  , name :: String
  , label :: String
  , attribs :: Array (VD.Property FormMsg)
  }

data InputTyp
  = IText
  | ICheckbox

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

form :: String -> Maybe String -> FormDefT -> FormDefT
form name legend content =
  liftF $ FormF { name, legend, content } unit

fieldset :: String -> Maybe String -> FormDefT -> FormDefT
fieldset name legend content =
  liftF $ FieldsetF { name, legend, content } unit

input :: InputTyp -> String -> String -> Array (VD.Property FormMsg) -> FormDefT
input typ name label attribs =
  liftF $ InputF { typ, name, label, attribs } unit

textInput :: String -> String -> Array (VD.Property FormMsg) -> FormDefT
textInput =
  input IText

checkboxInput :: String -> String -> Array (VD.Property FormMsg) -> FormDefT
checkboxInput =
  input ICheckbox

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
