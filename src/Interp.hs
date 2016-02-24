{-# LANGUAGE ScopedTypeVariables #-}

module Interp
  (
   getType,
   getType'
  ) where

import           Data.Typeable
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Interpreter as Hint
import qualified Text.Show.Pretty as Pr
import           Control.Monad



typeOfAST :: Typeable a => a -> ParseResult Type
typeOfAST = parseType . show . typeOf

foo :: ParseResult Type
foo = parseType "a -> a"


getType :: String -> IO (Either Hint.InterpreterError String)
getType = hrun . Hint.typeOf

getType' :: String -> IO (Either Hint.InterpreterError String)
getType' x = do
  x <- hrun . Hint.typeOf $ x
  return x


hrun :: Hint.Interpreter a -> IO (Either Hint.InterpreterError a)
hrun x = Hint.runInterpreter
  $ Hint.setImports ["Prelude"] 
  >> x

mon :: IO ()
mon = forever $ do
  putStr "λλλ: "
  l <- getLine
  elt :: Either Hint.InterpreterError String <- hrun . Hint.typeOf $ l
  case elt of
    Left _ -> putStrLn "error!"
    Right lt -> do
      let lt_ast :: ParseResult Type =  parseType lt
      putStrLn ""
      putStrLn lt
      putStrLn ""
      putStrLn $ Pr.ppShow lt_ast
