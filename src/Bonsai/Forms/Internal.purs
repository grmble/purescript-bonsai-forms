-- | Internal Module for the Forms DSL
module Bonsai.Forms.Internal
  ( Fieldset
  , Input
  , Grouped
  , CustomControl
  , InputTyp(..)
  , FormDef
  , FormDefF(..)
  , FormDefT
  , class HasAttribute
  , (!)
  , withAttribute
  , withAttributes
  )
where

import Prelude

import Bonsai.Forms.Model (FormMsg)
import Bonsai.Html (MarkupT)
import Bonsai.Html.Internal as HI
import Bonsai.VirtualDom (Property)
import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, hoistFree)
import Data.CatList as CL
import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)


--
--
-- dsl data structures
--
--

type Fieldset =
  { name :: String
  , legend :: Maybe String
  , attribs :: CL.CatList (Property FormMsg)
  , content :: FormDefT
  }

type Input =
  { typ :: InputTyp
  , name :: String
  , label :: String
  , message :: Maybe String
  , attribs :: CL.CatList (Property FormMsg)
  }

-- for radios and checkboxes
-- multiple controls with the same name
type Grouped =
  { typ :: InputTyp
  , name :: String
  , message :: Maybe String
  , attribs :: CL.CatList (Property FormMsg)
  , inputs :: Array (Tuple String String)
  }

type CustomControl =
  { name :: String
  , label :: String
  , message :: Maybe String
  , markup :: MarkupT FormMsg
  }

data InputTyp
  = IText
  | INumber
  | IColor
  | IEmail
  | IFile
  -- IHidden -- can be done as customElement
  -- IImage -- its a button with a custom image
  | IPassword
  | IRange
  | ISearch
  | ITel
  | IUrl

  | IDate
  | IMonth
  | IWeek
  | IDatetimeLocal
  | ITime

  | ICheckbox
  | IRadio

instance showInputTyp :: Show InputTyp where
  show i = case i of
    IText -> "text"
    INumber -> "number"
    IColor -> "color"
    IEmail -> "email"
    IFile -> "file"
    IPassword -> "password"
    IRange -> "range"
    ISearch -> "seach"
    ITel -> "tel"
    IUrl -> "url"

    IDate -> "date"
    IMonth -> "month"
    IWeek -> "week"
    IDatetimeLocal -> "datetime-local"
    ITime -> "time"

    ICheckbox -> "checkbox"
    IRadio -> "radio"

data FormDefF a
  = FormF Fieldset a
  | FieldsetF Fieldset a
  | InputF Input a
  | GroupedF Grouped a
  | CustomMarkupF (MarkupT FormMsg) a
  | CustomControlF CustomControl a
  | EmptyF a

instance functorFormDefF :: Functor FormDefF where
  map f (FormF s x) = FormF s (f x)
  map f (FieldsetF rec x) = FieldsetF rec (f x)
  map f (InputF v x) = InputF v (f x)
  map f (GroupedF g x) = GroupedF g (f x)
  map f (CustomMarkupF m x) = CustomMarkupF m (f x)
  map f (CustomControlF m x) = CustomControlF m (f x)
  map f (EmptyF x) = EmptyF (f x)

type FormDef = Free FormDefF
type FormDefT = FormDef Unit


--
--
-- HasAttribute / ! syntax like in Bonsai.Html.Internal
--
-- I'd prefer to reuse the type class, but it gives an orphan instance
--
class HasAttribute a b | a -> b where
  -- | Add an attribute to element node
  withAttribute :: a -> b -> a

instance hasAttributeFormDef :: HasAttribute (Free FormDefF Unit) (VD.Property FormMsg) where
  withAttribute elem prop =
    hoistFree go elem
    where
      go :: FormDefF ~> FormDefF
      go (FormF rec x) = FormF (rec { attribs = CL.snoc rec.attribs prop }) x
      go (FieldsetF rec x) = FieldsetF (rec { attribs = CL.snoc rec.attribs prop }) x
      go (InputF rec x) = InputF (rec { attribs = CL.snoc rec.attribs prop }) x
      go (GroupedF rec x) = GroupedF (rec { attribs = CL.snoc rec.attribs prop }) x
      go (CustomMarkupF markup x) = CustomMarkupF (markup HI.! prop) x
      go (CustomControlF rec x) = CustomControlF (rec { markup = rec.markup HI.! prop }) x
      go (EmptyF x) = EmptyF x

instance hasAttributeFormDefF :: HasAttribute (Free FormDefF Unit -> Free FormDefF Unit) (VD.Property FormMsg) where
  withAttribute efn prop elem =
    withAttribute (efn elem) prop


infixl 4 withAttribute as !


-- | Appends multiple Properties to a MarkupT
withAttributes
  :: MarkupT FormMsg
  -> CL.CatList (VD.Property FormMsg)
  -> MarkupT FormMsg
withAttributes elem attribs =
  foldl HI.withAttribute elem attribs
