module Prettify
    (
      Doc
    , empty
    , char
    , text
    , double
    , line
    , (<>)
    , hcat
    , fsep
    , (</>)
    , punctuate
    , group
    , softline
    , nest
    , block
    , compact
    , pretty
    ) where

import Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold _ [] = empty
fold f ds = foldr1 f ds

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- | Indenta em @i@ espaços tudo o que vier depois de cada quebra de
-- linha do documento. As quebras que ja foram "achatadas" (o lado
-- esquerdo de um 'Union') nao tem 'Line', entao nao sao afetadas.
nest :: Int -> Doc -> Doc
nest i doc
    | i <= 0    = doc
    | otherwise = go doc
  where
    pad = Text (replicate i ' ')
    go Line           = Line `Concat` pad
    go (a `Concat` b) = go a `Concat` go b
    go (a `Union` b)  = go a `Union` go b
    go other          = other

-- | Bloco delimitado (array/objeto): ou tudo em uma linha, ou um item
-- por linha, indentado em @i@ espacos, com o fechamento na sua propria
-- linha. O lado esquerdo do 'Union' e a versao achatada.
block :: Int -> Char -> Char -> [Doc] -> Doc
block _ open close [] = char open <> char close
block i open close ds = flat `Union` broken
  where
    flat   = flatten (char open <> sepBy (char ' ') ds <> char close)
    broken = char open <> nest i (line <> sepBy line ds)
                       <> line <> char close

-- | Intercala um separador entre os documentos, sem sobra no final
-- (diferente de @fold@, que cola o separador antes do 'empty').
sepBy :: Doc -> [Doc] -> Doc
sepBy _   [] = empty
sepBy sep ds = foldr1 (\x y -> x <> sep <> y) ds

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_ `fits` ""        = True
_ `fits` ('\n':_)  = True
w `fits` (_:cs)    = (w - 1) `fits` cs
