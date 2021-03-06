-- | Bonsai Forms DSL
-- |
-- | Provides a DSL for HTML5 forms.
-- |
-- | Name parameters are used to build up unique html id attributes,
-- | so they need name liternals - no spaces or funny characters
module Bonsai.Forms
  ( module InternalExports
  , module ModelExports
  , form
  , fieldset
  , withMessage
  , withLegend
  , textInput
  , numberInput
  , colorInput
  , emailInput
  , passwordInput
  , rangeInput
  , searchInput
  , telInput
  , urlInput
  , dateInput
  , datetimeLocalInput
  , monthInput
  , weekInput
  , timeInput
  , checkboxInput
  , radioInput
  , textareaInput

  , customMarkup
  , customControl
  , simpleSelectMarkup
  , simpleSelect
  )
where

import Prelude

import Bonsai.Forms.Internal (FormDef, FormDefF(..), InputTyp(..), Name)
import Bonsai.Forms.Internal (FormDef, Name, withAttribute, (!)) as InternalExports
import Bonsai.Forms.Model (FormModel, FormMsg, lookupChecked, targetSelectedOptions)
import Bonsai.Forms.Model (FormMsg, FormModel) as ModelExports
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.Types (Cmd(..))
import Control.Monad.Free (hoistFree, liftF)
import Data.CatList as CL
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)

--
--
-- Form DSL
--
--

-- | Paints a form element suitable for the internal definitions.
-- |
-- | Note that the interpreters try use html5 and it's internal
-- | validation as much as possible, so all the internal inputs
-- | and buttons assume that there will be an outer form
-- | with an onSubmit.
form :: Name -> FormDef -> FormDef
form name content =
  liftF $ FormF { name, content, legend: Nothing, attribs: CL.empty } unit

-- | A fieldset - a group of related controls.
fieldset :: Name -> FormDef -> FormDef
fieldset name content =
  liftF $ FieldsetF { name, content, legend: Nothing, attribs: CL.empty } unit

-- | Form Controls can optional messages.
withMessage :: FormDef -> String -> FormDef
withMessage elem s =
  hoistFree go elem
  where
    go :: FormDefF ~> FormDefF
    go (InputF i x) =
      InputF (i { message = Just s }) x
    go (GroupedF g x) =
      GroupedF (g { message = Just s }) x
    go (CustomControlF c x) =
      CustomControlF (c { message = Just s }) x
    go x = x

-- | Forms and Fieldsets can have optional legends.
withLegend :: (FormDef -> FormDef) -> String -> FormDef -> FormDef
withLegend efn s elem =
  hoistFree go (efn elem)
  where
    go :: FormDefF ~> FormDefF
    go (FieldsetF fs x) =
      FieldsetF (fs { legend = Just s }) x
    go (FormF frm x) =
      FormF (frm { legend = Just s }) x
    go x = x


input :: InputTyp -> Name -> String -> FormDef
input typ name label =
  liftF $ InputF { typ, name, label, attribs: CL.empty, message: Nothing } unit


grouped :: InputTyp -> Name -> Array (Tuple Name String) -> FormDef
grouped typ name inputs =
  liftF $ GroupedF { typ, name, inputs, attribs: CL.empty, message: Nothing } unit


-- | HTML text input
textInput :: Name -> String -> FormDef
textInput = input IText

-- | HTML5 number input, degrades to text.
-- |
-- | Support seems decent, all the major browsers.
numberInput :: Name -> String -> FormDef
numberInput = input INumber

-- | HTML5 color input, degrades to text.
colorInput :: Name -> String -> FormDef
colorInput = input IColor

-- | HTML5 email input, degrades to text.
emailInput :: Name -> String -> FormDef
emailInput = input IEmail

-- | HTML password input.
passwordInput :: Name -> String -> FormDef
passwordInput = input IPassword

-- | HTML5 range input, degrads to text.
rangeInput :: Name -> String -> FormDef
rangeInput = input IRange

-- | HTML5 search input, degrades to text.
searchInput :: Name -> String -> FormDef
searchInput = input ISearch

-- | HTML5 tel input, degrades to text.
telInput :: Name -> String -> FormDef
telInput = input ITel

-- | HTML5 url input, degrades to text.
urlInput :: Name -> String -> FormDef
urlInput = input IUrl

-- | HTML5 date input.
-- |
-- | Suport seems decent, all the major browsers except IE.
dateInput :: Name -> String -> FormDef
dateInput = input IDate

-- | HTML5 datetime-local input, degrades to text.
datetimeLocalInput :: Name -> String -> FormDef
datetimeLocalInput = input IDatetimeLocal

-- | HTML5 datetime-local input, degrades to text.
monthInput :: Name -> String -> FormDef
monthInput = input IMonth

-- | HTML5 datetime-local input, degrades to text.
weekInput :: Name -> String -> FormDef
weekInput = input IWeek

-- | HTML5 datetime-local input, degrades to text.
timeInput :: Name -> String -> FormDef
timeInput = input ITime


-- | HTML checkbox
checkboxInput :: Name -> Array (Tuple Name String) -> FormDef
checkboxInput =
  grouped ICheckbox

-- | HTML radio control
-- |
-- | You have to provide a default value in the model for this,
-- | or the control is going to start out-of-sync with the model
radioInput :: Name -> Array (Tuple Name String) -> FormDef
radioInput =
  grouped IRadio


-- | HTML textarea
textareaInput :: Name -> String -> FormDef
textareaInput =
  input ITextarea

--
--
-- custom X for extensions
--
--

-- | Custom markup will show up in the result as-is.
-- |
-- | Bad news is, the structure of the resulting html
-- | depends a lot on the DSL interpreter (aligned? compact?
-- | pure css? bootstrap?), so this will tie the form definition
-- | to that distinct use case.
customMarkup :: H.Markup FormMsg -> FormDef
customMarkup m =
  liftF $ CustomMarkupF m unit


-- | Custom controls will be rendered like Forms inputs.
-- |
-- | You have to provide a name and a label,
-- | the markup itself will be rendered for the label
-- | but unchanged except for the ID attribute - this will be set.
-- |
-- | You also also have to arrange for event handling.
customControl :: Name -> String -> (Name -> H.Markup FormMsg) -> FormDef
customControl name label markup =
  liftF $ CustomControlF { name, label, markup, message: Nothing } unit


-- | Helper for simple select elements via customControl
-- |
-- | For the easy cases, where there are no optgroups and such.
-- | The id will be set by `customControl`, you have to set
-- | any required or multiple attributes.
-- |
-- | If required is true, you have to provide a default value in
-- | the model, or the control and the model are going to start
-- | out-of-sync.
-- |
-- |        customControl name label $
-- |          -- if required is true, be sure to PROVIDE A DEFAULT VALUE in the model
-- |          map (\m -> m H.! required true) $
-- |              simpleSelectMarkup model [Tuple "o1" "Option 1", Tuple "o2" "Option 2"]
simpleSelectMarkup
  :: forall f
  .  Foldable f
  => FormModel
  -> f (Tuple String String)
  -> Name
  -> H.Markup FormMsg
simpleSelectMarkup model opts name =
  H.select
    H.! E.on "change" (map Cmd <<< targetSelectedOptions name)
    $ do
    -- option with empty value is so called "placeholder label option"
    -- H.option H.! A.value "" $ H.text "Select one ..."
    -- but works best WITHOUT "placeholder label option"
    -- model has to default one value from the options though, like radio
    for_ opts \opt ->
      H.option H.! A.value (fst opt)
        H.! A.selected (lookupChecked name (fst opt) model)
        $ H.text (snd opt)


-- | simple select control
-- |
-- | single select, not required
simpleSelect
  :: forall f
  .  Foldable f
  => Name
  -> String
  -> FormModel
  -> f (Tuple String String)
  -> FormDef
simpleSelect name label model opts=
  customControl name label $
    simpleSelectMarkup model opts
