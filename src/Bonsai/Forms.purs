module Bonsai.Forms
  ( Fieldset
  , Input
  , Grouped
  , InputTyp(..)
  , FormDef
  , FormDefF(..)
  , FormDefT
  , FormMsg(..)
  , FormModel(..)
  , form
  , fieldset
  , withMessage
  , withLegend
  , textInput
  , checkboxInput
  , radioInput
  , emptyFormModel
  , updateForm
  , set
  , insert
  , remove
  , setChecked
  , lookup
  , multiLookup
  , lookupChecked
  )
where

import Prelude

import Bonsai (UpdateResult, plainResult)
import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, hoistFree, liftF)
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)


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
  , message :: Maybe String
  , attribs :: Array (VD.Property FormMsg)
  }

-- for radios and checkboxes
-- multiple controls with the same name
type Grouped =
  { typ :: InputTyp
  , default :: Maybe String
  , name :: String
  , message :: Maybe String
  , inputs :: Array (Tuple String String)
  }

data InputTyp
  = IText
  | ICheckbox
  | IRadio

instance showInputTyp :: Show InputTyp where
  show (IText) =
    "text"

  show (ICheckbox) =
    "checkbox"

  show (IRadio) =
    "radio"


data FormDefF a
  = FormF Fieldset a
  | FieldsetF Fieldset a
  | InputF Input a
  | GroupedF Grouped a
  | EmptyF a

instance functorFormDefF :: Functor FormDefF where
  map f (FormF s x) = FormF s (f x)
  map f (FieldsetF rec x) = FieldsetF rec (f x)
  map f (InputF v x) = InputF v (f x)
  map f (GroupedF g x) = GroupedF g (f x)
  map f (EmptyF x) = EmptyF (f x)

type FormDef = Free FormDefF
type FormDefT = FormDef Unit

--
--
-- Form DSL
--
--

form :: String -> FormDefT -> FormDefT
form name content =
  liftF $ FormF { name, legend: Nothing, content } unit

fieldset :: String -> FormDefT -> FormDefT
fieldset name content =
  liftF $ FieldsetF { name, legend: Nothing, content } unit

input :: InputTyp -> String -> String -> Array (VD.Property FormMsg) -> FormDefT
input typ name label attribs =
  liftF $ InputF { typ, name, label, message: Nothing, attribs } unit

grouped :: InputTyp -> Maybe String -> String -> Array (Tuple String String) -> FormDefT
grouped typ default name inputs =
  liftF $ GroupedF { typ, default, name, message: Nothing, inputs } unit

withMessage :: FormDefT -> String -> FormDefT
withMessage elem s =
  hoistFree go elem
  where
    go :: FormDefF ~> FormDefF
    go (InputF i x) =
      InputF (i { message = Just s }) x
    go (GroupedF g x) =
      GroupedF (g { message = Just s }) x
    go x = x

withLegend :: (FormDefT -> FormDefT) -> String -> FormDefT -> FormDefT
withLegend efn s elem =
  hoistFree go (efn elem)
  where
    go :: FormDefF ~> FormDefF
    go (FieldsetF fs x) =
      FieldsetF (fs { legend = Just s }) x
    go (FormF frm x) =
      FormF (frm { legend = Just s }) x
    go x = x

textInput :: String -> String -> Array (VD.Property FormMsg) -> FormDefT
textInput =
  input IText

checkboxInput :: String -> Array (Tuple String String) -> FormDefT
checkboxInput =
  grouped ICheckbox Nothing

radioInput :: String -> String -> Array (Tuple String String) -> FormDefT
radioInput n def =
  grouped IRadio (Just def) n

--
--
-- form messages
--
--
data FormMsg
  = FormSet String String
  | FormAdd String String
  | FormRemove String String
  | FormCheck String String Boolean
  | FormOK
  | FormCancel

type FormModel =
  M.Map String (NEL.NonEmptyList String)


emptyFormModel :: FormModel
emptyFormModel =
  M.empty

updateForm :: forall eff. FormModel -> FormMsg -> UpdateResult eff FormModel FormMsg
updateForm model msg =
  plainResult $
    case msg of
      FormSet k v ->
        set k v model
      FormAdd k v ->
        insert k v model
      FormRemove k v ->
        remove k v model
      FormCheck k v b ->
        setChecked k v b model
      _ ->
        model

set :: String -> String -> FormModel -> FormModel
set k v model =
  M.insert k (NEL.singleton v) model

insert :: String -> String -> FormModel -> FormModel
insert k v model =
  M.alter (myAdd v) k model

  where
    myAdd s Nothing =
      Just $ NEL.singleton s
    myAdd s (Just nel) =
      Just $ NEL.cons s nel

remove :: String -> String -> FormModel -> FormModel
remove k v model =
  M.alter (myRemove v) k model

  where
    myRemove s (Nothing) =
      Nothing
    myRemove s (Just nel) =
      NEL.fromList $ L.delete s (NEL.toList nel)

lookup :: String -> FormModel -> Maybe String
lookup k model =
  NEL.head <$> M.lookup k model

multiLookup :: String -> FormModel -> L.List String
multiLookup k model =
  case M.lookup k model of
    Nothing ->
      L.Nil
    Just nel ->
      NEL.toList nel

setChecked :: String -> String -> Boolean -> FormModel -> FormModel
setChecked k v b model =
  if b then insert k v model else remove k v model

lookupChecked :: String -> String -> FormModel -> Boolean
lookupChecked k v model =
  L.elem v (multiLookup k model)
