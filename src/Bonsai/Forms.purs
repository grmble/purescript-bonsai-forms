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
  )
where

import Prelude

import Bonsai.Forms.Internal (FormDefF(..), FormDefT, InputTyp(..))
import Bonsai.Forms.Internal (withAttribute, (!)) as InternalExports
import Bonsai.Forms.Model (FormMsg, FormModel) as ModelExports
import Control.Monad.Free (hoistFree, liftF)
import Data.CatList as CL
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

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
form :: String -> FormDefT -> FormDefT
form name content =
  liftF $ FormF { name, content, legend: Nothing, attribs: CL.empty } unit

-- | A fieldset - a group of related controls.
fieldset :: String -> FormDefT -> FormDefT
fieldset name content =
  liftF $ FieldsetF { name, content, legend: Nothing, attribs: CL.empty } unit

-- | Form Controls can optional messages.
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

-- | Forms and Fieldsets can have optional legends.
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


input :: InputTyp -> String -> String -> FormDefT
input typ name label =
  liftF $ InputF { typ, name, label, attribs: CL.empty, message: Nothing } unit


grouped :: InputTyp -> String -> Array (Tuple String String) -> FormDefT
grouped typ name inputs =
  liftF $ GroupedF { typ, name, inputs, attribs: CL.empty, message: Nothing } unit


-- | HTML text input
textInput :: String -> String -> FormDefT
textInput = input IText

-- | HTML5 number input, degrades to text.
-- |
-- | Support seems decent, all the major browsers.
numberInput :: String -> String -> FormDefT
numberInput = input INumber

-- | HTML5 color input, degrades to text.
colorInput :: String -> String -> FormDefT
colorInput = input IColor

-- | HTML5 email input, degrades to text.
emailInput :: String -> String -> FormDefT
emailInput = input IEmail

-- | HTML password input.
passwordInput :: String -> String -> FormDefT
passwordInput = input IPassword

-- | HTML5 range input, degrads to text.
rangeInput :: String -> String -> FormDefT
rangeInput = input IRange

-- | HTML5 search input, degrades to text.
searchInput :: String -> String -> FormDefT
searchInput = input ISearch

-- | HTML5 tel input, degrades to text.
telInput :: String -> String -> FormDefT
telInput = input ITel

-- | HTML5 url input, degrades to text.
urlInput :: String -> String -> FormDefT
urlInput = input IUrl

-- | HTML5 date input.
-- |
-- | Suport seems decent, all the major browsers except IE.
dateInput :: String -> String -> FormDefT
dateInput = input IDate

-- | HTML5 datetime-local input, degrades to text.
datetimeLocalInput :: String -> String -> FormDefT
datetimeLocalInput = input IDatetimeLocal

-- | HTML5 datetime-local input, degrades to text.
monthInput :: String -> String -> FormDefT
monthInput = input IMonth

-- | HTML5 datetime-local input, degrades to text.
weekInput :: String -> String -> FormDefT
weekInput = input IWeek

-- | HTML5 datetime-local input, degrades to text.
timeInput :: String -> String -> FormDefT
timeInput = input ITime


-- | HTML checkbox
checkboxInput :: String -> Array (Tuple String String) -> FormDefT
checkboxInput =
  grouped ICheckbox

-- | HTML radio control
radioInput :: String -> Array (Tuple String String) -> FormDefT
radioInput =
  grouped IRadio
