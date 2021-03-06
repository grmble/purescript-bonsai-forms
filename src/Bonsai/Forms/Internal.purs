-- | Internal Module for the Forms DSL
module Bonsai.Forms.Internal
  ( Name
  , Fieldset
  , Input
  , Grouped
  , CustomControl
  , InputTyp(..)
  , FormDef
  , FormDefF(..)
  , class HasAttribute
  , (!)
  , withAttribute
  )
where

import Prelude

import Bonsai.Forms.Model (FormMsg)
import Bonsai.Html (Markup)
import Bonsai.Html.Internal as HI
import Bonsai.VirtualDom (Property)
import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, hoistFree)
import Data.CatList as CL
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)


--
--
-- dsl data structures
--
--

-- | String suitable for a html name attribute
-- |
-- | Note that the DSL joins nested Names to
-- | build html id attributes.  So do not
-- | use any characters you would not use in
-- | such an attribute.
type Name = String

type Fieldset =
  { name :: Name
  , legend :: Maybe String
  , attribs :: CL.CatList (Property FormMsg)
  , content :: FormDef
  }

type Input =
  { typ :: InputTyp
  , name :: Name
  , label :: String
  , message :: Maybe String
  , attribs :: CL.CatList (Property FormMsg)
  }

-- for radios and checkboxes
-- multiple controls with the same name
type Grouped =
  { typ :: InputTyp
  , name :: Name
  , message :: Maybe String
  , attribs :: CL.CatList (Property FormMsg)
  , inputs :: Array (Tuple Name String)
  }

type CustomControl =
  { name :: Name
  , label :: String
  , message :: Maybe String
  , markup :: Name -> Markup FormMsg
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

  -- not an html input, but for our DSL it's the same
  | ITextarea

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

    ITextarea -> "textarea"



data FormDefF a
  = FormF Fieldset a
  | FieldsetF Fieldset a
  | InputF Input a
  | GroupedF Grouped a
  | CustomMarkupF (Markup FormMsg) a
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

type FormDef = Free FormDefF Unit


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
      go (CustomControlF rec x) = CustomControlF (rec { markup = map (\m -> m HI.! prop) rec.markup }) x
      go (EmptyF x) = EmptyF x

instance hasAttributeFormDefF :: HasAttribute (Free FormDefF Unit -> Free FormDefF Unit) (VD.Property FormMsg) where
  withAttribute efn prop elem =
    withAttribute (efn elem) prop


infixl 4 withAttribute as !
