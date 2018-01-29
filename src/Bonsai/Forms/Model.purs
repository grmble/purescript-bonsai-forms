-- | Bonsai Forms Model
-- |
-- | Simple map based form model - because you ususually
-- | can't just use your domain model as backing
-- | (e.g. invalid user input - it still has to be displayed
-- | to the user and offered for correction)
module Bonsai.Forms.Model
  ( FormMsg(..)
  , FormModel(..)
  , emptyFormModel
  , updateForm
  , updatePlain
  , insert
  , insertMulti
  , removeMulti
  , updateChecked
  , lookup
  , lookupMulti
  , lookupChecked
  , targetSelectedOptions
  )
where

import Prelude

import Bonsai (Cmd, plainResult)
import Data.Array as A
import Data.Foreign (F, Foreign, readInt, readString)
import Data.Foreign.Index ((!))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)

--
--
-- form messages
--
--

-- | Form Messages
data FormMsg
  = FormSingle String String
  | FormMulti String String
  | FormRemove String String
  | FormRemoveAll String
  | FormCheck String String Boolean
  | FormOK
  | FormCancel

-- | Form Model
-- |
-- | It can store multiple values for a key
-- | to match the way checkboxes/selects work.
-- |
-- | Single-value inputs just work with the first value.
type FormModel =
  M.Map String (NEL.NonEmptyList String)


-- | Produces an empty form model.
emptyFormModel :: FormModel
emptyFormModel =
  M.empty

-- | plain update function for a FormModel
-- |
-- | already using the planned msg model arguments
updatePlain :: FormMsg -> FormModel -> FormModel
updatePlain msg model =
  case msg of
    FormSingle k v ->
      insert k v model
    FormMulti k v ->
      insertMulti k v model
    FormRemove k v ->
      removeMulti k v model
    FormRemoveAll k ->
      removeAll k model
    FormCheck k v b ->
      updateChecked k v b model
    _ ->
      model

-- | update function for a FormModel
updateForm :: forall eff. FormMsg -> FormModel -> Tuple (Cmd eff FormMsg) FormModel
updateForm =
  map plainResult <<< updatePlain


-- | Insert a single value for the key.
-- |
-- | This will set the single value as the only
-- | value for the key.
insert :: String -> String -> FormModel -> FormModel
insert k v model =
  M.insert k (NEL.singleton v) model

-- | Insert a value for a multi-valued key.
-- |
-- | This will add this value as an additional value.
insertMulti :: String -> String -> FormModel -> FormModel
insertMulti k v model =
  -- sometimes the checkboxes/radios are bugging out
  -- we want to keep the model clean ...
  M.alter (myAdd v) k
    (removeMulti k v model)

  where
    myAdd s Nothing =
      Just $ NEL.singleton s
    myAdd s (Just nel) =
      Just $ NEL.cons s nel

-- | Remove a value from a multi-valued key.
removeMulti :: String -> String -> FormModel -> FormModel
removeMulti k v model =
  M.alter (myRemove v) k model

  where
    myRemove s (Nothing) =
      Nothing
    myRemove s (Just nel) =
      NEL.fromList $ L.filter (\x -> s /= x) (NEL.toList nel)

-- | Remove all the values for the key
removeAll :: String -> FormModel -> FormModel
removeAll k model =
  M.alter (const Nothing) k model

-- | Lookup the (single) value for a key.
lookup :: String -> FormModel -> Maybe String
lookup k model =
  NEL.head <$> M.lookup k model

-- | Lookup all values for a multi-valued key.
lookupMulti :: String -> FormModel -> L.List String
lookupMulti k model =
  case M.lookup k model of
    Nothing ->
      L.Nil
    Just nel ->
      NEL.toList nel

-- | Update or remove a value from a multi-valued key.
-- |
-- | This is a checkbox helper, it allows to add or
-- | remove a value from a set-like list.
updateChecked :: String -> String -> Boolean -> FormModel -> FormModel
updateChecked k v b model =
  (if b then insertMulti else removeMulti) k v model

-- | Lookup a checkbox value in the form model.
lookupChecked :: String -> String -> FormModel -> Boolean
lookupChecked k v model =
  L.elem v (lookupMulti k model)

-- | event decoder for select elements
targetSelectedOptions :: String -> Foreign -> F (Array FormMsg)
targetSelectedOptions name event = do
  optsDomList <- event ! "target" ! "selectedOptions"
  len <- optsDomList ! "length" >>= readInt
  opts <- traverse (\i -> optsDomList ! i ! "value" >>= readString) (L.range 0 (len - 1))
  case opts of
    L.Nil ->
      pure [FormRemoveAll name]
    L.Cons s1 L.Nil ->
      pure [FormSingle name s1]
    _ -> do
      pure $ A.fromFoldable $ L.Cons (FormRemoveAll name) (map (FormMulti name) opts)
