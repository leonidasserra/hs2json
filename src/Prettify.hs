module Prettify where

import Prelude hiding ((<>))

data Doc = ToBeDefined
         deriving (Show)

text :: String -> Doc
text _ = undefined

double :: Double -> Doc
double _ = undefined

char :: Char -> Doc
char _ = undefined

(<>) :: Doc -> Doc -> Doc
_ <> _ = undefined

hcat :: [Doc] -> Doc
hcat _ = undefined

fsep :: [Doc] -> Doc
fsep _ = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
