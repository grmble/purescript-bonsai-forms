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
  , Prop(..)
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
  , toProperty
  , combineClasses
  )
where

import Prelude

import Bonsai (UpdateResult, plainResult)
import Bonsai.Html.Attributes as HA
import Bonsai.Html.Internal as HI
import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, hoistFree, liftF)
import Data.Array as A
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
  , props :: Array Prop -- only ClassList
  , content :: FormDefT
  }

type Input =
  { typ :: InputTyp
  , name :: String
  , label :: String
  , message :: Maybe String
  , props :: Array Prop
  }

-- for radios and checkboxes
-- multiple controls with the same name
type Grouped =
  { typ :: InputTyp
  , name :: String
  , message :: Maybe String
  , props :: Array Prop -- only ClassList
  , inputs :: Array (Tuple String String)
  }

data Prop
  = Required Boolean
  | Disabled Boolean
  | Readonly Boolean
  | Pattern String
  | ClassList (Array String)
  | Autocomplete String

data InputTyp
  = IText
  | ICheckbox
  | IRadio

instance showInputTyp :: Show InputTyp where
  show i = case i of
    IText -> "text"
    ICheckbox -> "checkbox"
    IRadio -> "radio"

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
  liftF $ FormF { name, content, legend: Nothing, props: [] } unit

fieldset :: String -> FormDefT -> FormDefT
fieldset name content =
  liftF $ FieldsetF { name, content, legend: Nothing, props: [] } unit

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

withClasses :: (FormDefT -> FormDefT) -> Array String -> FormDefT -> FormDefT
withClasses efn cs elem =
  hoistFree go (efn elem)
  where
    go :: FormDefF ~> FormDefF
    go e = case e of
      FieldsetF a x -> FieldsetF (a { props = A.snoc a.props (ClassList cs)}) x
      FormF a x -> FormF (a { props = A.snoc a.props (ClassList cs)}) x
      GroupedF a x -> GroupedF (a { props = A.snoc a.props (ClassList cs)}) x
      InputF a x -> InputF (a { props = A.snoc a.props (ClassList cs)}) x
      EmptyF x -> EmptyF x

input :: InputTyp -> String -> String -> Array Prop -> FormDefT
input typ name label props =
  liftF $ InputF { typ, name, label, props, message: Nothing } unit


grouped :: InputTyp -> String -> Array (Tuple String String) -> FormDefT
grouped typ name inputs =
  liftF $ GroupedF { typ, name, inputs, props: [], message: Nothing } unit


textInput :: String -> String -> Array Prop -> FormDefT
textInput =
  input IText

checkboxInput :: String -> Array (Tuple String String) -> FormDefT
checkboxInput =
  grouped ICheckbox

radioInput :: String -> Array (Tuple String String) -> FormDefT
radioInput =
  grouped IRadio

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

--
--
-- helpers for interpreters
--
--
combineClasses :: Array String -> Array Prop -> VD.Property FormMsg
combineClasses as =
  HA.cls <<<
  A.intercalate " " <<<
  (A.union as <<< A.concat <<< map extractClasses)

  where
    extractClasses :: Prop -> Array String
    extractClasses (ClassList cs) = cs
    extractClasses _ = []

toProperty :: Prop -> VD.Property FormMsg
toProperty p =
  case p of
    Required b -> HA.required b
    Disabled b -> HA.disabled b
    Readonly b -> HA.readonly b
    Pattern s -> HA.pattern s
    ClassList cs -> HA.cls (A.intercalate " " cs)
    Autocomplete s -> HI.stringProperty "autocomplete" s
    -- novalidate?
    -- spellcheck?

    -- the following via dsl primitives (different for text, ...)
    -- max, min, maxlength, list, multiple (file/email)


-- XXX need better name for these model functions
-- checkbox models operate like sets, but those
-- functions are named insert/remove.
-- set isnt a set, but it sets ... confused yet?
set :: String -> String -> FormModel -> FormModel
set k v model =
  M.insert k (NEL.singleton v) model

insert :: String -> String -> FormModel -> FormModel
insert k v model =
  -- sometimes the checkboxes/radios are bugging out
  -- we want to keep the model clean ...
  M.alter (myAdd v) k
    (remove k v model)

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
      NEL.fromList $ L.filter (\x -> s /= x) (NEL.toList nel)


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
