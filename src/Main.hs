module Main (main) where
import LangProp(Identifier(..), LangProp(..))
import Naive
import Parser
import qualified CNF as C 
import qualified LangPropCore as L

test = C.transform . L.transform . testProp

main :: IO ()
main = do
  putStrLn "hello world"
