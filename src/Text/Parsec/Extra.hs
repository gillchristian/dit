module Text.Parsec.Extra where

import Control.Monad (void)
import qualified Text.Parsec as P
-- TODO: use generic parser instead
import Text.Parsec.Text (Parser)

checkWordEnd :: Parser ()
checkWordEnd = P.notFollowedBy P.alphaNum

lineSpaces :: Parser ()
lineSpaces = void $ P.many $ P.oneOf " \t"
