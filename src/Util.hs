module Util where

import Data.Either
import Text.Parsec
import Text.Parsec.String


loadParse :: Parser a -> FilePath -> IO a
loadParse p = fmap (either (error . show) id . runParser p () "") . readFile
