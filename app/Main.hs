module Main where

import SimpleJSON
import Prettify
import PrettyJSON

main :: IO()
main = do
  let value = renderJValue (JObject [("nome", JString "Sergio Costa"), ("matricula", JNumber 123), ("status", JBool True)])
  putStrLn(pretty 10 value)