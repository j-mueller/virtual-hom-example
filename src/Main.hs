{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Lens hiding (children)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           VirtualHom.Components
import           VirtualHom.Element(attributes, callbacks, children, content, value)
import qualified VirtualHom.Element as E
import qualified VirtualHom.Html as H
import           VirtualHom.Internal.Handler (update)
import           VirtualHom.Rendering(renderingOptions)

newtype FirstName = FirstName { getFirstName :: Text }
newtype LastName = LastName { getLastName :: Text }

validFirstName :: Text -> Either Text FirstName
validFirstName t = if T.length t > 0 then Right (FirstName t) else Left "First name must not be empty" 

validLastName :: Text -> Either Text LastName
validLastName t = if T.length t > 0 then Right (LastName t) else Left "Last name must not be empty"

data Person = Person {
  _firstName :: FirstName,
  _lastName  :: LastName
}
makeLenses ''Person


validatingTextInput :: (Text -> Either Text a) -> Component (Maybe a)
validatingTextInput f = component "" $ return . result where
  errorMarker = either (const "has-error") (const "") . f
  validated = either (const Nothing) Just . f
  result (textValue, a) = H.div
    & attributes . at "class" ?~ ("form-group " <> errorMarker textValue)
    & children .~ [
        H.input
          & attributes . at "class" ?~ "form-control"
          & attributes . at "type"  ?~ "text"
          & attributes . at "value" ?~ textValue
          & callbacks . E.change ?~ (\e -> update $ const (e^.value, validated $ e^.value))
    ]

data PersonForm = PersonForm {
  _fn :: Maybe FirstName,
  _ln :: Maybe LastName,
  _firstNameInput :: Component (Maybe FirstName),
  _lastNameInput  :: Component (Maybe LastName) 
}
makeLenses ''PersonForm

personForm :: PersonForm
personForm = PersonForm Nothing Nothing i1 i2 where
  i1 = validatingTextInput validFirstName
  i2 = validatingTextInput validLastName

personComp :: Component ()
personComp = component personForm $ \tpl -> [ container & children .~ [
  row & children .~ [H.h1 "Edit contact"],
  row & children .~ subComponent (state.fn) (state.firstNameInput) tpl,
  row & children .~ subComponent (state.ln) (state.lastNameInput)  tpl
  ] <> (maybe [] personLabel $ tpl^.state.to person)]

personLabel :: Person -> [E.Elem cb ()]
personLabel p = [row & children .~ [
  H.div 
    & attributes . at "class" ?~ "alert alert-success"
    & content .~ "Hello, " <> personText p
  ]]

main :: IO ()
main = do
  let options = renderingOptions "virtual-hom"
  renderComponent options personComp ()

-- | Utilities

personText :: Person -> Text
personText p = p^.firstName.to getFirstName <> " " <> p^.lastName.to getLastName

person :: PersonForm -> Maybe Person
person pf = Person <$> view fn pf <*> view ln pf 

-- | Bootstrap CSS
container = H.div & attributes . at "class" ?~ "container"
row = H.div & attributes . at "class" ?~ "row"
