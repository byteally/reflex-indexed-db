{-# LANGUAGE OverloadedStrings #-}

import Reflex.IDB
import Reflex.Dom
import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Monad.IO.Class
import qualified GHCJS.DOM.Enums as DOM

testIDB :: IO ()
testIDB = mainWidget $ do
  closeE <- button "Close DB"
  todoAddE <- el "div" $ do
    toDotxt <- textInput def
    addE <- button "+"
    let todoAddE = tagPromptlyDyn (_textInput_value toDotxt) addE
    return (todoAddE)
  let idbOpenReq = IndexedDBOpen (T.pack "foo") 1 closeE
  idbE <- indexedDB idbOpenReq $ do
    todoS <- createObjectStore (T.pack "todo") Nothing
--    deleteObjectStore (T.pack "todo")
    return ()
  let idb = (unSafeRight idbE)
  tres <- runTransaction idb (TransactionConfig ["todo"] DOM.IDBTransactionModeReadwrite todoAddE never) $ do
    todoSt <- openStore "todo"
    inp <- getInput
    add todoSt (A.String "testV") (Just $ inp)
    add todoSt (A.String "testV") (Just $ inp <> "_1")
    liftIO $ print inp
    return ()
  text "Is DB Open: "
  dynText (fmap (T.pack . show . not . isClosed) $ _idb_state $ unSafeRight idbE)
  dynText =<< (holdDyn "Upgarding.." $ case idbE of
    Left e  -> never
    Right r -> fmap (T.pack . show) $ _idb_onUpgrading r)
  dynText =<< (holdDyn "trans ..." $ ffor tres $ \tres' -> case tres' of
                  Left e -> "Trans err: " <> T.pack (show e)
                  Right r -> "trans done")
  return ()
    where unSafeRight (Right r) = r

main :: IO ()
main = testIDB
