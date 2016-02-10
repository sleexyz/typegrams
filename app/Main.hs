module Main where

import           Control.Monad
import           Data.Typeable
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Interpreter as Hint
import qualified Text.Show.Pretty as Pr


typeOfAST :: Typeable a => a -> ParseResult Type
typeOfAST = parseType . show . typeOf

foo :: ParseResult Type
foo = parseType "a -> a"


hrun :: Hint.Interpreter a -> IO (Either Hint.InterpreterError a)
hrun x = Hint.runInterpreter $
  Hint.setImports ["Prelude"] >> x


main :: IO ()
main = forever $ do
  putStr "λλλ: "
  l <- getLine
  elt <- hrun . Hint.typeOf $ l
  case elt of
    Left _ -> putStrLn "error!"
    Right lt -> do
      let lt_ast =  parseType lt
      putStrLn ""
      putStrLn lt
      putStrLn ""
      putStrLn $ Pr.ppShow lt_ast
