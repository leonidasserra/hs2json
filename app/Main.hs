module Main (main) where

import SimpleJSON
import PutJSON (putJValue)
import PrettyJSON (renderJValue)
import Prettify (compact, pretty)
import System.IO (hSetEncoding, stdout, utf8)

sample :: JValue
sample = JObject
    [ ("nome",     JString "Leônidas")
    , ("curso",    JString "Engenharia da Computação")
    , ("periodo",  JNumber 7)
    , ("ativo",    JBool True)
    , ("tags",     JArray [JString "haskell", JString "json", JString "quickcheck"])
    , ("endereco", JObject [ ("cidade", JString "São Luís")
                           , ("uf",     JString "MA") ])
    , ("escapes",  JString "tab:\t quebra:\n aspas:\" emoji:\x1F600")
    , ("conjuge",  JNull)
    ]

main :: IO ()
main = do
    hSetEncoding stdout utf8

    putStrLn "== Renderizador ingenuo (PutJSON) =="
    putJValue sample

    putStrLn "\n== compact =="
    putStrLn (compact (renderJValue sample))

    putStrLn "\n== pretty, largura 80 =="
    putStrLn (pretty 80 (renderJValue sample))

    putStrLn "\n== pretty, largura 30 =="
    putStrLn (pretty 30 (renderJValue sample))

    putStrLn "\n== Accessors =="
    print (getString (JString "ola"))
    print (getInt (JNumber 3.99))
    print (getBool (JNumber 1))
    print (fmap length (getObject sample))
    print (isNull JNull)
