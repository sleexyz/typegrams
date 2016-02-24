{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}


module Main where

import           Control.Monad
import           Data.Typeable
import           Control.Concurrent (forkIO)
import           Reflex.Dom
import           Reflex.Dom.Class
import           Control.Monad.IO.Class
import           Data.Function
import           Interp (eat)



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
f x = fmap g (eat $ x)  where
    g (Left e) = show e
    g (Right str) = str
  











-- utils

infixl 1 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- From https://github.com/reflex-frp/reflex-dom/pull/11/files
asyncMapIO :: (MonadWidget t m) => (a -> IO b) -> Event t a -> m (Event t b)
asyncMapIO f event = performEventAsync eActions
     where eActions = fmap (forkApply f) event
           forkApply f val callback = liftIO . void . forkIO $ f val >>= callback
  
say :: (MonadIO m) => String -> m ()
say = liftIO . putStrLn
