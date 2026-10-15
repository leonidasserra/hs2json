{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Prelude hiding ((<>))

import Control.Monad (liftM, liftM2)
import System.Exit (exitFailure)
import Test.QuickCheck
import SimpleJSON
import Data.Maybe (isJust)
import Data.List (intersperse)
import Data.Char (chr, isHexDigit)
import Numeric (readHex)
import PrettyJSON (renderJValue)

import Prettify

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

-- | Gerador de JValue com profundidade E largura limitadas.
-- O 'resize' em volta de 'listOf' e obrigatorio: sem ele, o tamanho
-- da lista segue o parametro AMBIENTE do QuickCheck (ate 100) e a
-- arvore explode em largura, travando o processo.
instance Arbitrary JValue where
    arbitrary = sized genJValue

    shrink (JString s)  = JNull : map JString (shrink s)
    shrink (JNumber n)  = JNull : map JNumber (shrink n)
    shrink (JBool b)    = JNull : map JBool (shrink b)
    shrink JNull        = []
    shrink (JArray vs)  = JNull : vs ++ map JArray (shrink vs)
    shrink (JObject kv) = JNull : map snd kv ++ map JObject (shrink kv)

genJValue :: Int -> Gen JValue
genJValue n
    | n <= 0    = leaf
    | otherwise = oneof [leaf, recursive]
  where
    leaf = oneof
        [ JString <$> arbitrary
        , JNumber <$> arbitrary
        , JBool   <$> arbitrary
        , pure JNull
        ]
    smaller   = genJValue (n `div` 2)
    field     = (,) <$> arbitrary <*> smaller
    recursive = oneof
        [ JObject <$> resize (n `div` 2) (listOf field)
        , JArray  <$> resize (n `div` 2) (listOf smaller)
        ]

prop_empty_id :: Doc -> Bool
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char :: Char -> Bool
prop_char c = char c == Char c

prop_text :: String -> Bool
prop_text s = text s == if null s then Empty else Text s

prop_line :: Bool
prop_line = line == Line

prop_double :: Double -> Bool
prop_double d = double d == text (show d)

prop_hcat :: [Doc] -> Bool
prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate' :: Doc -> [Doc] -> Bool
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys

prop_compact_text :: String -> Bool
prop_compact_text s = compact (text s) == s

-- | Cada accessor devolve exatamente o conteudo do construtor certo.
prop_accessors_correct :: JValue -> Bool
prop_accessors_correct v = case v of
    JString s -> getString v == Just s
    JNumber n -> getDouble v == Just n && getInt v == Just (truncate n)
    JBool b   -> getBool v == Just b
    JNull     -> isNull v
    JObject o -> getObject v == Just o
    JArray a  -> getArray v == Just a

-- | Para qualquer valor, exatamente UM accessor tem sucesso.
prop_accessors_exclusive :: JValue -> Bool
prop_accessors_exclusive v = length (filter id checks) == 1
  where
    checks = [ isJust (getString v), isJust (getDouble v)
             , isJust (getBool v),   isJust (getObject v)
             , isJust (getArray v),  isNull v ]

-- | getInt e getDouble concordam (getInt = truncate . getDouble).
prop_getInt_consistent :: JValue -> Bool
prop_getInt_consistent v = getInt v == fmap truncate (getDouble v)

renderString :: String -> String
renderString = compact . renderJValue . JString

-- | A saida so contem caracteres seguros: nada de controle,
-- nada de DEL e nada acima de U+00FF (esses viram \uXXXX).
prop_escape_safe_chars :: String -> Bool
prop_escape_safe_chars s = all safe (renderString s)
  where safe c = c >= ' ' && c /= '\DEL' && c <= '\xff'

-- | Desfazer os escapes devolve exatamente a string original
-- (inclusive emojis, que viram pares substitutos).
prop_escape_roundtrip :: String -> Bool
prop_escape_roundtrip s = unescape (renderString s) == Just s

-- | Decodificador de referencia, usado apenas nos testes.
unescape :: String -> Maybe String
unescape ('"':rest) = go rest
  where
    go ['"']           = Just []
    go ('"':_)         = Nothing          -- aspas sem escape no meio
    go ('\\':'u':cs)   = do
        (h, cs') <- hex4 cs
        if h >= 0xd800 && h < 0xdc00
            then case cs' of
                ('\\':'u':cs2) -> do
                    (l, cs3) <- hex4 cs2
                    let c = chr (0x10000 + (h - 0xd800) * 0x400 + (l - 0xdc00))
                    (c :) <$> go cs3
                _ -> Nothing
            else (chr h :) <$> go cs'
    go ('\\':c:cs)     = do
        r <- lookup c [ ('b','\b'), ('n','\n'), ('f','\f'), ('r','\r')
                      , ('t','\t'), ('\\','\\'), ('"','"'), ('/','/') ]
        (r :) <$> go cs
    go (c:cs)          = (c :) <$> go cs
    go []              = Nothing
    hex4 cs = case splitAt 4 cs of
        (h, r) | length h == 4 && all isHexDigit h ->
                   case readHex h of
                       [(n, "")] -> Just (n, r)
                       _         -> Nothing
        _ -> Nothing
unescape _ = Nothing

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    passed <- runTests
    if passed
        then putStrLn "Passou em todos os testes."
        else do putStrLn "Alguns testes falharam."
                exitFailure