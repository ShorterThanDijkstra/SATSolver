module Main (main) where
import LangProp(Identifier(..), LangProp(..))
import Naive
import Parser
import TruthTable ( table, TruthTable )
import qualified CNF as C 
import qualified LangPropCore as L

-- test = C.transform . L.transform . parseProp'
test :: String -> TruthTable
test = table . parseProp'

main :: IO ()
main = do
  putStrLn "hello world"
