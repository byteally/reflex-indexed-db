-- | 

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.IDB where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHCJS.DOM
import qualified GHCJS.DOM.IDBDatabase as IDBD
import qualified GHCJS.DOM.IDBFactory as IDBF
import qualified GHCJS.DOM.IDBOpenDBRequest as IDBOReq
import qualified GHCJS.DOM.IDBRequest as IDBReq
import qualified GHCJS.DOM.IDBTransaction as IDBTrans
import qualified GHCJS.DOM.IDBObjectStore as IDBStore
import qualified GHCJS.DOM.IDBCursor as IDBCurs
import qualified GHCJS.DOM.IDBKeyRange as IDBKeyRan
import qualified GHCJS.DOM.JSFFI.Window as FFIWin
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.EventM
import Control.Monad
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Ref
import Reflex
import Reflex.Dom
import Reflex.Host.Class
import Reflex.Dom.Class
import Data.Text (Text)
import Data.Proxy
import GHC.TypeLits
import Data.Default
import Data.Word
import Data.Dependent.Map (DSum (..))
import Data.Functor.Identity
import Data.IORef
import Control.Monad.Trans.Free
import Control.Monad.Trans.State hiding (get)
import Data.Aeson as A

-- Exp
import qualified Data.Text as T
import GHCJS.DOM.JSFFI.Generated.WindowTimers
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal
--


-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Convert a 'Maybe' value into the 'ExceptT' monad
(??) :: Applicative m => Maybe a -> e -> ExceptT e m a
(??) a e = ExceptT (pure $ note e a)

-- | Convert an applicative 'Maybe' value into the 'ExceptT' monad
(!?) :: Applicative m => m (Maybe a) -> e -> ExceptT e m a
(!?) a e = ExceptT (note e <$> a)

type IDBError = Text
newtype IDBResult t a = IDBResult (Event t (Either IDBError a))

indexedDB :: forall t m.
            ( HasWebView m
            , MonadWidget t m
            ) => IndexedDBOpen t -> (forall t1. (Reflex t1) => Database t1 IO ()) -> m (Either IDBError (IndexedDB t))
indexedDB idbReq upgrade' = do
  let upgrade = upgrade' :: Database t IO ()
  wv <- askWebView
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
  (eUpgrade, eUpgradeTriggerRef) <- newEventWithTriggerRef
  idbRef <- liftIO $ newIORef Nothing
  let onBlocked :: IO ()
      onBlocked = return ()
      onUpgradeNeeded :: IO (Either Text ()) -> IO ()
      onUpgradeNeeded upgardeRes = do
        -- TODO: Probably needs to generalize IO to WidgetHost m
        --       Requires a fn WidgetHost m (Either Text ()) -> IO (Either Text ())
        res <- upgardeRes
        postGui $ do
          mt <- readRef eUpgradeTriggerRef
          forM_ mt $ \t -> runWithActions [t :=> Identity res]
          return ()
      onSuccess :: IndexedDBState -> IO ()
      onSuccess idbSt = postGui $ do
        mt <- readRef eOpenTriggerRef
        forM_ mt $ \t -> runWithActions [t :=> Identity idbSt]
        return ()
      onError :: IO ()
      onError = do
        print "error openining db"
        return ()
  res <- liftIO $ runExceptT $ do   
--  res <- either (return Nothing) (Just <$> schedulePostBuild) $ liftIO $ runExceptT $ do
    wind <- currentWindow !? "Unable to get window object"
    idbFact <- FFIWin.getIndexedDB wind !? "IndexedDB not supported"
    idbOpenReq <- IDBF.open idbFact idbName idbVer !? "Unable to open IndexedDB"
    _ <- liftIO $ on idbOpenReq IDBOReq.blocked $ liftIO onBlocked
    _ <- liftIO $ on idbOpenReq IDBReq.success $ do
      tgt <- target
      let idbReq = maybe (error "Error getting idb request") id tgt :: IDBReq.IDBRequest
      idbAny <- liftIO $ IDBReq.getResult idbReq
      let idb = maybe (error "Error getting idb") IDBD.castToIDBDatabase idbAny
      liftIO $ writeIORef idbRef (Just idb)
      liftIO (onSuccess .  Open . IDBRef =<< newIORef idb)
    _ <- liftIO $ on idbOpenReq IDBReq.error $ liftIO onError
    _ <- liftIO $ on idbOpenReq IDBOReq.upgradeNeeded $ do
      tgt <- target
      let idbReq = maybe (error "Error getting idb request") id tgt :: IDBReq.IDBRequest
      idbAny <- liftIO $ IDBReq.getResult idbReq
      let idb = maybe (error "Error getting idb") IDBD.castToIDBDatabase idbAny
      let upgradeCode = runDatabase upgrade
      let res = runExceptT (iterT (interpertDB idb) upgradeCode)
      liftIO $ onUpgradeNeeded res
      return ()
    return ()
  closeE <- performEvent $ ffor (_idb_close idbReq) $ \_ -> do
    idbM <- liftIO $ readIORef idbRef
    case idbM of
      Just idb -> do
        IDBD.close idb
        liftIO $ print "Closing........."
        liftIO $ modifyIORef idbRef (const Nothing)
        return Close
      Nothing -> return Close
  let mergedStatus = leftmost [closeE, eOpen]
      isClosedE    = fmap isClosed mergedStatus    
      isOpenE      = fmap not isClosedE
  idbState <- holdDyn Close mergedStatus
  return $ case res of
    Left e -> Left (T.pack e)
    Right _ -> Right $ IndexedDB isOpenE idbState eUpgrade
  where idbName = _idb_name idbReq
        idbVer  = _idb_version idbReq
        interpertDB idb (OpCreateObjectStore storeN opts f) = do
          liftIO $ print "OpCreateObjectStore"
          store <- IDBD.createObjectStore idb storeN (Nothing :: Maybe DOM.Dictionary) !? (nullErr "createObjectStore" [])
          f (ObjectStore store)

        interpertDB idb (OpDeleteObjectStore storeN f) = do
          IDBD.deleteObjectStore idb storeN
          f


isClosed :: IndexedDBState -> Bool
isClosed Close = True
isClosed _     = False

data IndexedDBOpen t = IndexedDBOpen
  { _idb_name    :: Text
  , _idb_version :: Word64
  , _idb_close :: Event t ()
  }


newtype IDBRef = IDBRef {runIDBRef :: IORef IDBD.IDBDatabase}
data IndexedDBState
  = Open IDBRef 
  | Close
    
data IndexedDB t = forall item.IndexedDB
  { _idb_isOpen          :: Event t Bool
  , _idb_state           :: Dynamic t IndexedDBState
  , _idb_onUpgrading     :: Event t (Either Text ())
  }

data TransactionMode = ReadWrite
                     | ReadOnly
                     | ReadWriteFlush
                     | VersionChange
                     deriving (Show, Read)
                              
data NewObjectStore t
  = StoreWithAutoKey Text
  | StoreWithKeyPath Text

data IndexParam = IndexParam
  { isUnique :: Bool
  , isMultiEntry :: Bool
  , locale :: Text
  }
  
type NewIndex = (Text, Text, IndexParam)

data KeyRange = UpperBound Text Bool
              | LowerBound Text Bool
              | Bound (Text, Bool) (Text, Bool)
              | Only Text

data CursorMove = Next
                | NextUnique
                | Prev
                | PrevUnique
                  

type KeyOrKeyRange = Either Text KeyRange

newtype ObjectStore t = ObjectStore {runStore :: DOM.IDBObjectStore}

type Dict = ()

data DatabaseOp t item (m :: * -> *) f
  = OpCreateObjectStore Text (Maybe Dict) (ObjectStore t -> f)
  | OpDeleteObjectStore Text f
--  | OpStore (StoreOp t item m f) f
--  | OpTransaction (IDB t m item) f
    deriving (Functor)

newtype Database t m a = Database {runDatabase :: FreeT (DatabaseOp t Item m) (ExceptT IDBError m) a}
                       deriving ( Functor
                                , Applicative
                                , Monad
                                , MonadIO
                                )
                                
createObjectStore :: Monad m => Text -> Maybe Dict -> Database t m (ObjectStore t)
createObjectStore store opts = Database $ liftF $ OpCreateObjectStore store opts id

deleteObjectStore :: Monad m => Text -> Database t m ()
deleteObjectStore store = Database $ liftF $ OpDeleteObjectStore store ()

type St = ()
data StoreOp t item m f
  = OpOpenStore Text (ObjectStore t -> f)
  | OpAdd (ObjectStore t) item (Maybe Text) f
  | OpClear (ObjectStore t) f
  | OpCount (ObjectStore t) (Maybe KeyOrKeyRange) (Either IDBError Int -> f)
  | OpNewIndex (ObjectStore t) NewIndex f
  | OpDelete (ObjectStore t) Text f
  | OpDeleteIndex (ObjectStore t) Text f
  | OpGet (ObjectStore t) Text (Either IDBError (Maybe Text) -> f)
  | OpGetAll (ObjectStore t) (Maybe KeyOrKeyRange) (Maybe Int) (Either IDBError [Text] -> f)
  | OpGetAllKeys (ObjectStore t) (Maybe KeyRange) (Maybe Int) (Either IDBError [Text] -> f)
  | OpIndex (ObjectStore t) Text f
  | OpCursor (ObjectStore t) (Maybe KeyOrKeyRange) (Maybe CursorMove) () (Cursor t St m ()) f
  | OpKeyCursor (ObjectStore t) (Maybe (KeyRange)) (Maybe CursorMove) (Cursor t St m ()) f
  | OpPut (ObjectStore t) item (Maybe Text) f
    deriving (Functor)

data CursorOp t item f
  = OpContinue f
  | OpDone f
  | OpAdvance Int f
  | OpCurrent (item -> f)
  | OpCurUpdate item f
  | OpCurDelete f
  deriving (Functor)

-- newtype IDBCursor t = IDBCursor {runCursor :: IDBCurs.IDBCursor}
continue :: (Monad m) => Cursor t s m ()
continue = Cursor $ liftF $ OpContinue ()

done :: (Monad m) => Cursor t s m ()
done = Cursor $ liftF $ OpDone ()

advance :: (Monad m) => Int -> Cursor t s m ()
advance n = Cursor $ liftF $ OpAdvance n ()

currentValue :: (Monad m) => Cursor t s m Item
currentValue = Cursor $ liftF $ OpCurrent id

currentUpdate :: (Monad m) => Item -> Cursor t s m ()
currentUpdate item = Cursor $ liftF $ OpCurUpdate item ()

currentDelete :: (Monad m) => Cursor t s m ()
currentDelete = Cursor $ liftF $ OpCurDelete ()

type Item = Text

newtype Cursor t s m a = Cursor { runCursor :: FreeT (CursorOp t Item) (ExceptT IDBError (StateT s m)) a}
                       deriving ( Functor
                                , Applicative
                                , Monad
                                , MonadIO
                                )

newtype IDB t r m a = IDB { runIDB :: FreeT (StoreOp t Item m) (ExceptT IDBError (ReaderT r m)) a}
                  deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadIO
                           )

openStore :: (Monad m) => Text -> IDB t r m (ObjectStore t)
openStore store = IDB $ liftF $ OpOpenStore store id

add :: (Monad m) => (ObjectStore t) -> Item -> Maybe Text -> IDB t r m ()
add store item key = IDB $ liftF $ OpAdd store item key ()

clear :: (Monad m) => (ObjectStore t) -> IDB t r m ()
clear store = IDB $ liftF $ OpClear store ()

count :: (Monad m) => (ObjectStore t) -> Maybe KeyOrKeyRange -> IDB t r m (Either IDBError Int)
count store key = IDB $ liftF $ OpCount store key id

get :: (Monad m) => (ObjectStore t) -> Text -> IDB t r m (Either IDBError (Maybe Text))
get store key = IDB $ liftF $ OpGet store key id

getAll :: (Monad m) => (ObjectStore t) -> Maybe KeyOrKeyRange -> Maybe Int -> IDB t r m (Either IDBError [Text])
getAll store key count = IDB $ liftF $ OpGetAll store key count id

openCursor :: (Monad m) => (ObjectStore t) -> Maybe KeyOrKeyRange -> (Maybe CursorMove) -> () -> (Cursor t St m ()) -> IDB t r m ()
openCursor store key dir initState curCode = IDB $ liftF $ OpCursor store key dir initState curCode ()

data TransactionConfig t a = TransactionConfig
  { _transCfg_scopes  :: [Text]
  , _transCfg_mode    :: TransactionMode
  , _transCfg_trigger :: Event t a
  , _transCfg_abort   :: Event t ()
  }

nullErr :: String -> [String] -> Text
nullErr op args = T.pack $ op ++ " returned null, when called with args" ++ (show args)

runTransaction :: forall t m input output.
  ( MonadIO m
  , MonadSample t m
  , MonadWidget t m
--  , input ~ ()
  ) => IndexedDB t -> TransactionConfig t input -> (forall t1. (Reflex t1) => IDB t1 input (WidgetHost m) output) -> m (Event t (Either IDBError output))
runTransaction idb transCfg transact = do
  let trigger = _transCfg_trigger transCfg
      fixT = const :: IDB t input m1 o -> Event t a -> IDB t input m1 o
      code = transact `fixT` trigger
      scopes = case _transCfg_scopes transCfg of
        (s : _) -> s
        []      -> error $ "Atleast one trasanction scopes are required"
      transMode = showTransactionMode $ _transCfg_mode transCfg
  performEvent $ ffor trigger $ \input -> do
    idbState <- sample $ current $ _idb_state idb
    case idbState of
      Close -> return $ Left $ T.pack "Closed"
      Open idbRef' -> do
        let idbRef = runIDBRef idbRef'
        idb <- liftIO $ readIORef idbRef
        transM <- IDBD.transaction idb scopes transMode
        case transM of
          Nothing -> return $ Left $ T.pack "Unable to create transaction"
          Just trans -> runReaderT (runExceptT $ iterT (interpret input trans) $ runIDB code) input
  where
    interpret input idbTrans (OpOpenStore storeN f) = do
      objStore <- IDBTrans.objectStore idbTrans storeN !? (nullErr "objectStore" [])
      f $ ObjectStore objStore
    interpret input idbTrans (OpAdd (ObjectStore store) item key f) = do
      itemVal <- liftIO $ toJSVal item
      keyVal <- liftIO $ toJSVal key
      liftIO $ print $ "adding " ++ (show key)
      req <- IDBStore.add store itemVal keyVal !? (nullErr "add" [])
      f

    interpret input idbTrans (OpClear (ObjectStore store) f) = do
      req <- IDBStore.clear store !? (nullErr "clear" [])
      f

    interpret input idbTrans (OpCount (ObjectStore store) key f) = do
      req <- case key of
        Nothing -> do
          IDBStore.countRange store Nothing !? (nullErr "countRange" [])
        Just (Left keyName) -> do
          keyVal <- liftIO $ toJSVal keyName
          IDBStore.count store keyVal !? (nullErr "count" [])
        Just (Right krange) -> do
          case krange of
            UpperBound key' b -> do
              keyVal <- liftIO $ toJSVal key'
              range <- IDBKeyRan.upperBound (error "TODO") keyVal b !? (nullErr "upperBound" [])
              IDBStore.countRange store (Just range) !? (nullErr "countRange" [])
            LowerBound key' b -> do
              keyVal <- liftIO $ toJSVal key'
              range <- IDBKeyRan.lowerBound (error "TODO") keyVal b !? (nullErr "lowerBound" [])
              IDBStore.countRange store (Just range) !? (nullErr "countRange" [])
            Bound (k1, b1) (k2, b2) -> do
              kVal1 <- liftIO $ toJSVal k1
              kVal2 <- liftIO $ toJSVal k2
              range <- IDBKeyRan.bound (error "TODO") kVal1 kVal2 b1 b2 !? (nullErr "bound" [])
              IDBStore.countRange store (Just range) !? (nullErr "countRange" [])
            Only k -> do
              kVal <- liftIO $ toJSVal k
              range <- IDBKeyRan.only (error "TODO") kVal !? (nullErr "only" [])
              IDBStore.countRange store (Just range) !? (nullErr "countRange" [])
      f (Left $ T.pack "TODO")

    interpret input idbTrans (OpGet (ObjectStore store) key f) = do
      keyVal <- liftIO $ toJSVal key
      req <- IDBStore.get store keyVal !? (nullErr "get" [])
      f (Left $ T.pack "TODO")
      
    interpret input idbTrans (OpGetAll (ObjectStore store) key kcount f) = do
      error "getAll is not available in GHCJS"
      
    interpret input idbTrans (OpGetAllKeys (ObjectStore store) key kcount f) = do
      error "getAllKeys is not available in GHCJS"
      
    interpret input idbTrans (OpIndex (ObjectStore store) idx f) = do
      req <- IDBStore.index store idx !? (nullErr "index" [])
      f
    interpret input idbTrans (OpNewIndex (ObjectStore store) (idxName, key, idxPar) f) = do
      idxNameVal <- liftIO $ toJSVal idxName
      idx <- IDBStore.createIndex store idxName key (Nothing :: Maybe DOM.Dictionary) !? (nullErr "createIndex" [])
      f
    interpret input idbTrans (OpDelete (ObjectStore store) key f) = do
      keyVal <- liftIO $ toJSVal key
      req <- IDBStore.delete store keyVal !? (nullErr "delete" [])
      f
    interpret input idbTrans (OpDeleteIndex (ObjectStore store) key f) = do
      req <- IDBStore.deleteIndex store key
      f
    interpret input idbTrans (OpCursor (ObjectStore store) key' move init curOps f) = do
      let dirStr = showCursorMove $ maybe Next id move
      req <- case key' of
        Nothing -> do
          IDBStore.openCursorRange store Nothing dirStr !? (nullErr "openCursorRange" [])
        Just (Left key) -> do
          keyVal <- liftIO $ toJSVal key
          IDBStore.openCursor store keyVal dirStr !? (nullErr "openCursor" [])
        Just (Right krange) -> do
          case krange of
            UpperBound key' b -> do
              keyVal <- liftIO $ toJSVal key'
              range <- IDBKeyRan.upperBound (error "TODO") keyVal b !? (nullErr "upperBound" [])
              IDBStore.openCursorRange store (Just range) dirStr !? (nullErr "openCursorRange" [])
            LowerBound key' b -> do
              keyVal <- liftIO $ toJSVal key'
              range <- IDBKeyRan.lowerBound (error "TODO") keyVal b !? (nullErr "lowerBound" [])
              IDBStore.openCursorRange store (Just range) dirStr !? (nullErr "openCursorRange" [])
            Bound (k1, b1) (k2, b2) -> do
              kVal1 <- liftIO $ toJSVal k1
              kVal2 <- liftIO $ toJSVal k2
              range <- IDBKeyRan.bound (error "TODO") kVal1 kVal2 b1 b2 !? (nullErr "bound" [])
              IDBStore.openCursorRange store (Just range) dirStr !? (nullErr "openCursorRange" [])
            Only k -> do
              kVal <- liftIO $ toJSVal k
              range <- IDBKeyRan.only (error "TODO") kVal !? (nullErr "only" [])
              IDBStore.openCursorRange store (Just range) dirStr !? (nullErr "openCursorRange" [])
      let cursorRes = runStateT (runExceptT $ iterT (interpretCursor undefined) (runCursor curOps)) init
      f

    interpret input idbTrans (OpKeyCursor (ObjectStore store) key move curOps f) = do
      req <- IDBStore.openCursorRange store Nothing "<dir>" !? (nullErr "openCursorRange" [])
      f
    interpret input idbTrans (OpPut (ObjectStore store) item key f) = do
      keyVal <- liftIO $ toJSVal key
      itemVal <- liftIO $ toJSVal item
      req <- IDBStore.put store itemVal keyVal !? (nullErr "openCursorRange" [])
      f
      
    interpretCursor curs (OpContinue f) = do
      f

getInput :: Monad m => IDB t r m r
getInput = IDB ((lift . lift) ask)

showCursorMove :: CursorMove -> String
showCursorMove Next       = "next"
showCursorMove NextUnique = "nextunique"
showCursorMove Prev       = "prev"
showCursorMove PrevUnique = "prevunique"

showTransactionMode :: TransactionMode -> String
showTransactionMode ReadWrite = "readwrite"
showTransactionMode ReadWriteFlush  = "readwriteflush"
showTransactionMode ReadOnly = "readonly"
showTransactionMode VersionChange = "versionchange"

