module Data.Text.ToText (ToText (..)) where

import Data.Text (Text)

class ToText a where
  toText :: a -> Text
