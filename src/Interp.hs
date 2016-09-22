module Interp
    ( InterpTask (Run, TypeCheck),
      evalHS,
    ) where

import Control.Monad
import Language.Haskell.Interpreter
import Data.List

data InterpTask = Run | TypeCheck

evalHS :: InterpTask -> String -> IO (Either [String] String)
evalHS task hsexpr = do
  let taskfn = case task of
        Run -> eval
        TypeCheck -> typeOf
  let interpreter = do
        setImportsQ $ myPackages `zip` (repeat Nothing)
        taskfn hsexpr
  r <- runInterpreter interpreter
  case r of
    Left err -> return $ Left $ errorString err
    Right result -> return $ Right result

errorString :: InterpreterError -> [String]
errorString (WontCompile es) = (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = [show e]

myPackages :: [String]
myPackages = [
  "Prelude", 
  "Data.Function", 
  "Control.Applicative", 
  "Control.Arrow", 
  "Data.Array", 
  "Data.Bits", 
  "Data.Bool", 
  "Data.Char", 
  "Data.Complex", 
  "Data.Dynamic", 
  "Data.Either", 
  "Data.Eq", 
  "Data.Fixed", 
  "Data.Graph", 
  "Data.Int", 
  "Data.Ix", 
  "Data.List", 
  "Data.Maybe", 
  "Data.Monoid",
  "Data.Ord",
  "Data.Ratio",
  "Data.Tree",
  "Data.Tuple",
  "Data.Typeable",
  "Data.Word"]
  --"System.Random",
  --"Text.PrettyPrint.HughesPJ",
  --"Text.Printf" ]
