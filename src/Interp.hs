{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Interp
  (
   getType,
   getType',
   TypeSig

  ) where

import           Data.Typeable
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Interpreter as Hint
import qualified Text.Show.Pretty as Pr
import           Control.Monad
import           GHC.Generics



-- typeOfAST :: Typeable a => a -> ParseResult Type
-- typeOfAST = parseType . show . typeOf

-- foo :: ParseResult Type
-- foo = parseType "a -> a"


getType :: String -> IO (Either Hint.InterpreterError String)
getType = hrun . Hint.typeOf


type TypeSig = ParseResult Type
deriving instance Generic (ParseResult a)

getType' :: String -> IO (Either Hint.InterpreterError TypeSig)
getType' x = fmap f $ hrun . Hint.typeOf $ x
  where
    f (Left l) = Left l
    f (Right r) = Right . parseType $ r


hrun :: Hint.Interpreter a -> IO (Either Hint.InterpreterError a)
hrun x = Hint.runInterpreter
  $ Hint.setImports ["Prelude"]
  >> x

-- mon :: IO ()
-- mon = forever $ do
--   putStr "λλλ: "
--   l <- getLine
--   elt :: Either Hint.InterpreterError String <- hrun . Hint.typeOf $ l
--   case elt of
--     Left _ -> putStrLn "error!"
--     Right lt -> do
--       let lt_ast :: ParseResult Type =  parseType lt
--       putStrLn ""
--       putStrLn lt
--       putStrLn ""
--       putStrLn $ Pr.ppShow lt_ast
