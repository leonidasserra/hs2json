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