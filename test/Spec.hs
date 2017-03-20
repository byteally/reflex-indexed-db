
import Reflex.IDB
import Reflex.Dom
import qualified Data.Text as T
import Control.Monad.IO.Class

testIDB :: IO ()
testIDB = mainWidget $ do
  closeE <- button "Close DB"
  todoAddE <- el "div" $ do
    toDotxt <- textInput def
    addE <- button "+"
    let todoAddE = tagDyn (_textInput_value toDotxt) addE
    return (todoAddE)
  let idbOpenReq = IndexedDBOpen (T.pack "foo") 1 closeE
  idbE <- indexedDB idbOpenReq $ do
    todoS <- createObjectStore (T.pack "todo") Nothing
--    deleteObjectStore (T.pack "todo")
    return ()
  let idb = (unSafeRight idbE)
  tres <- runTransaction idb (TransactionConfig [T.pack "todo"] ReadWrite todoAddE never) $ do
    todoSt <- openStore (T.pack "todo")
    inp <- getInput
    add todoSt (T.pack "testV") (Just $ T.pack inp)
    add todoSt (T.pack "testV") (Just $ T.pack $ inp ++ "_1")
    liftIO $ print inp
    return ()
  dynText =<< (holdDyn "Waiting.." $ case idbE of
    Left e  -> never
    Right r -> fmap show $ _idb_isOpen r)
  dynText =<< (holdDyn "Upgarding.." $ case idbE of
    Left e  -> never
    Right r -> fmap show $ _idb_onUpgrading r)
  dynText =<< (holdDyn "trans ..." $ ffor tres $ \tres' -> case tres' of
                  Left e -> "Trans err: " ++ show e
                  Right r -> "trans done")
  return ()
    where unSafeRight (Right r) = r

main :: IO ()
main = testIDB
