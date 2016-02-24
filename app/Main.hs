{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}


module Main where

import           Control.Monad
import           Data.Typeable
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Interpreter as Hint
import qualified Text.Show.Pretty as Pr
import           Control.Concurrent (forkIO)
import           Reflex.Dom
import           Reflex.Dom.Class
import           Control.Monad.IO.Class
import           Control.Monad.Fix (MonadFix)
import           Data.Function



main :: IO ()
main = mainWidget myWidget


myWidget :: MonadWidget t m => m ()
myWidget = el "div" $ do

  txtIn      :: TextInput t        <- textInput def
  buttonEv   :: Event t ()         <- button "Submit"
  let textEv :: Event t String      = tagDyn (_textInput_value txtIn) buttonEv

  textDyn    :: Dynamic t String   <- holdDyn "enter something" textEv

  el "div" $ do
    el "h2" $ text "input:"
    dynText textDyn

  el "div" $ do
    el "h2" $ text "type sig:"
    tyEv    :: Event t String <- asyncMapIO f textEv
    tyDyn   :: Dynamic t String <- holdDyn "(type signature)" tyEv
    dynText tyDyn
    return ()
  return ()



f :: String -> IO String
f x = fmap g (hrun . Hint.typeOf $ x)  where
    g (Left e) = show e
    g (Right str) = str
  





-- Parsing

typeOfAST :: Typeable a => a -> ParseResult Type
typeOfAST = parseType . show . typeOf

foo :: ParseResult Type
foo = parseType "a -> a"


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
      let lt_ast =  parseType lt
      putStrLn ""
      putStrLn lt
      putStrLn ""
      putStrLn $ Pr.ppShow lt_ast








-- utils

infixl 1 <&>
(<&>) = flip fmap

-- From https://github.com/reflex-frp/reflex-dom/pull/11/files
asyncMapIO :: (MonadWidget t m) => (a -> IO b) -> Event t a -> m (Event t b)
asyncMapIO f event = performEventAsync eActions
     where eActions = fmap (forkApply f) event
           forkApply f val callback = liftIO . void . forkIO $ f val >>= callback
  
say :: (MonadIO m) => String -> m ()
say = liftIO . putStrLn
