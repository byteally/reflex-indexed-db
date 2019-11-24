-- |

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Reflex.IDB where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Ref
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State  hiding (get)
import           Data.Aeson                 as A
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Default
import           Data.Dependent.Map         (DSum (..))
import           Data.Functor.Identity
import           Data.IORef
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Word
import           GHC.TypeLits
import           GHCJS.DOM
import qualified GHCJS.DOM.Enums            as DOM
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.IDBCursor        as IDBCurs
import qualified GHCJS.DOM.IDBDatabase      as IDBD
import qualified GHCJS.DOM.IDBFactory       as IDBF
import qualified GHCJS.DOM.IDBKeyRange      as IDBKeyRan
import qualified GHCJS.DOM.IDBObjectStore   as IDBStore
import qualified GHCJS.DOM.IDBOpenDBRequest as IDBOReq
import qualified GHCJS.DOM.IDBRequest       as IDBReq
import qualified GHCJS.DOM.IDBTransaction   as IDBTrans
import qualified GHCJS.DOM.Types            as DOM
import qualified GHCJS.DOM.Window           as FFIWin
import           Prelude                    hiding ((!!))
import           Reflex
import           Reflex.Dom.Class
import           Reflex.Dom.Core
import           Reflex.Host.Class



-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Convert a 'Maybe' value into the 'ExceptT' monad
(??) :: Applicative m => Maybe a -> e -> ExceptT e m a
(??) a e = ExceptT (pure $ note e a)

-- | Convert an applicative 'Maybe' value into the 'ExceptT' monad
(!?) :: Applicative m => m (Maybe a) -> e -> ExceptT e m a
(!?) a e = ExceptT (note e <$> a)

-- | Convert an applicative 'Maybe' to a panic
(!!) :: Applicative m => m (Maybe a) -> String -> m a
(!!) a e = fmap (fromMaybe (error e)) a

type IDBError = Text
newtype IDBResult t a = IDBResult (Event t (Either IDBError a))

indexedDB :: forall t m.
            ( HasWebView m
            , MonadWidget t m
            ) => IndexedDBOpen t -> (Database t DOM.JSM ()) -> m (Either IDBError (IndexedDB t))
indexedDB idbReq upgrade = do
  (eOpen, eOpenTrigger) <- newTriggerEvent
  (eUpgrade, eUpgradeTrigger) <- newTriggerEvent
  idbRef <- liftIO $ newIORef Nothing
  let onBlocked :: DOM.JSM ()
      onBlocked = return ()
      onUpgradeNeeded :: DOM.JSM (Either Text ()) -> DOM.JSM ()
      onUpgradeNeeded upgardeRes = do
        -- TODO: Probably needs to generalize IO to WidgetHost m
        --       Requires a fn WidgetHost m (Either Text ()) -> IO (Either Text ())
        res <- upgardeRes
        liftIO $ eUpgradeTrigger res
      onSuccess :: IndexedDBState -> DOM.JSM ()
      onSuccess idbSt = liftIO $ eOpenTrigger idbSt
      onError :: DOM.JSM ()
      onError = do
        liftIO $ print "error openining db"
        return ()
  res <- DOM.liftJSM $ runExceptT $ do
    wind <- currentWindow !? "Unable to get window object"
    idbFact <- FFIWin.getIndexedDB wind
    idbOpenReq <- IDBF.open idbFact idbName (Just idbVer)
    _ <- DOM.liftJSM $ on idbOpenReq IDBOReq.blocked $ DOM.liftJSM onBlocked
    _ <- DOM.liftJSM $ on idbOpenReq IDBReq.success $ do
      tgt <- target
      let idbReq = maybe (error "Error getting idb request") id tgt :: IDBReq.IDBRequest
      idbAny <- IDBReq.getResult idbReq !! "Error getting Database"
      idb <- DOM.castTo IDBD.IDBDatabase idbAny !! "Error converting to database"
      liftIO $ writeIORef idbRef (Just idb)
      DOM.liftJSM (onSuccess .  Open . IDBRef =<< liftIO (newIORef idb))
    _ <- DOM.liftJSM $ on idbOpenReq IDBReq.error $ DOM.liftJSM onError
    _ <- DOM.liftJSM $ on idbOpenReq IDBOReq.upgradeNeeded $ do
      idbReq :: IDBReq.IDBRequest <- target !! "Error getting idb request"
      idbAny <- (IDBReq.getResult idbReq) !! "Error getting database"
      idb <- DOM.castTo IDBD.IDBDatabase idbAny !! "Error getting idb"
      let upgradeCode = runDatabase upgrade
      let res = runExceptT (iterT (interpretDB idb) upgradeCode)
      DOM.liftJSM $ onUpgradeNeeded res
      return ()
    return ()
  closeE <- performEvent $ ffor (_idb_close idbReq) $ \_ -> do
    idbM <- liftIO $ readIORef idbRef
    case (idbM :: Maybe DOM.IDBDatabase) of
      Just idb -> do
        _ <- IDBD.close idb
        liftIO $ print "Closing........."
        liftIO $ modifyIORef idbRef (const Nothing)
        return Close
      Nothing -> return Close
  let mergedStatus = leftmost [closeE, eOpen]
      isClosedE    = fmap isClosed mergedStatus
      isOpenE      = fmap not isClosedE
  idbState <- holdDyn Close mergedStatus
  return $ case res of
    Left e  -> Left (T.pack e)
    Right _ -> Right $ IndexedDB isOpenE idbState eUpgrade
  where idbName = _idb_name idbReq
        idbVer  = _idb_version idbReq


interpretDB
  :: DOM.IDBDatabase
  -> DatabaseOp t Item DOM.JSM (ExceptT IDBError DOM.JSM ())
  -> ExceptT IDBError DOM.JSM ()
interpretDB idb (OpCreateObjectStore storeN opts f) = do
  liftIO $ print "OpCreateObjectStore"
  store <- IDBD.createObjectStore idb storeN (Nothing :: Maybe DOM.IDBObjectStoreParameters)
  f (ObjectStore store)
interpretDB idb (OpDeleteObjectStore storeN f) = do
  IDBD.deleteObjectStore idb storeN
  f
interpretDB idb (OpIDB act f) =
  ExceptT $ flip runReaderT (T.pack "tmp") $ runExceptT $ iterT (interpret (T.pack "tmp") undefined) $ runIDB act


isClosed :: IndexedDBState -> Bool
isClosed Close = True
isClosed _     = False

data IndexedDBOpen t = IndexedDBOpen
  { _idb_name    :: Text
  , _idb_version :: Word64
  , _idb_close   :: Event t ()
  }


newtype IDBRef = IDBRef {runIDBRef :: IORef IDBD.IDBDatabase}
data IndexedDBState
  = Open IDBRef
  | Close

data IndexedDB t = forall item.IndexedDB
  { _idb_isOpen      :: Event t Bool
  , _idb_state       :: Dynamic t IndexedDBState
  , _idb_onUpgrading :: Event t (Either Text ())
  }

data NewObjectStore t
  = StoreWithAutoKey Text
  | StoreWithKeyPath Text

data IndexParam = IndexParam
  { isUnique     :: Bool
  , isMultiEntry :: Bool
  , locale       :: Text
  }

type NewIndex = (Text, Text, Maybe IndexParam)

data KeyRange = UpperBound Text Bool
              | LowerBound Text Bool
              | Bound (Text, Bool) (Text, Bool)
              | Only Text


type KeyOrKeyRange = Either Text KeyRange

newtype ObjectStore t = ObjectStore {runStore :: DOM.IDBObjectStore}

type Dict = ()

data DatabaseOp t item (m :: * -> *) f
  = OpCreateObjectStore Text (Maybe Dict) (ObjectStore t -> f)
  | OpDeleteObjectStore Text f
--  | OpStore (StoreOp t item m f) f
--  | OpTransaction (IDB t m item) f
  | OpIDB (IDB t Text m ()) f
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

runIDBAction :: Monad m => IDB t Text m () -> Database t m ()
runIDBAction act = Database $ liftF $ OpIDB act ()

type St = ()
data StoreOp t item m f
  = OpOpenStore Text            (ObjectStore t -> f)
  | OpAdd         (ObjectStore t) item (Maybe Text) f
  | OpClear       (ObjectStore t) f
  | OpCount       (ObjectStore t) (Maybe KeyOrKeyRange) (Either IDBError Int -> f)
  | OpNewIndex    (ObjectStore t) NewIndex f
  | OpDelete      (ObjectStore t) Text f
  | OpDeleteIndex (ObjectStore t) Text f
  | OpGet         (ObjectStore t) Text (Either IDBError (Maybe item) -> f)
  | OpGetAll      (ObjectStore t) (Maybe KeyOrKeyRange) (Maybe Int) (Either IDBError [item] -> f)
  | OpGetAllKeys  (ObjectStore t) (Maybe KeyRange) (Maybe Int) (Either IDBError [Text] -> f)
  | OpIndex       (ObjectStore t) Text f
  | OpCursor      (ObjectStore t) (Maybe KeyOrKeyRange) (Maybe DOM.IDBCursorDirection) () (Cursor t St m ()) f
  | OpKeyCursor   (ObjectStore t) (Maybe KeyRange)      (Maybe DOM.IDBCursorDirection) (Cursor t St m ()) f
  | OpPut         (ObjectStore t)                   item (Maybe Text) f
    deriving (Functor)

data CursorOp t item f
  = OpContinue  f
  | OpDone      f
  | OpAdvance   Int f
  | OpCurrent   (item -> f)
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

type Item = A.Value

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

instance DOM.MonadJSM m => DOM.MonadJSM (IDB t r m) where
  liftJSM' = IDB . FreeT . liftM Pure . DOM.liftJSM

openStore :: (Monad m) => Text -> IDB t r m (ObjectStore t)
openStore store = IDB $ liftF $ OpOpenStore store id

createIndex :: (Monad m) => ObjectStore t -> NewIndex -> IDB t r m ()
createIndex store newIndex = IDB $ liftF $ OpNewIndex store newIndex ()

add :: (Monad m) => (ObjectStore t) -> Item -> Maybe Text -> IDB t r m ()
add store item key = IDB $ liftF $ OpAdd store item key ()

clear :: (Monad m) => (ObjectStore t) -> IDB t r m ()
clear store = IDB $ liftF $ OpClear store ()

count :: (Monad m) => (ObjectStore t) -> Maybe KeyOrKeyRange -> IDB t r m (Either IDBError Int)
count store key = IDB $ liftF $ OpCount store key id

get :: (Monad m) => (ObjectStore t) -> Text -> IDB t r m (Either IDBError (Maybe Item))
get store key = IDB $ liftF $ OpGet store key id

getAll :: (Monad m) => (ObjectStore t) -> Maybe KeyOrKeyRange -> Maybe Int -> IDB t r m (Either IDBError [Item])
getAll store key count = IDB $ liftF $ OpGetAll store key count id

openCursor :: (Monad m) => (ObjectStore t) -> Maybe KeyOrKeyRange -> (Maybe DOM.IDBCursorDirection) -> () -> (Cursor t St m ()) -> IDB t r m ()
openCursor store key dir initState curCode = IDB $ liftF $ OpCursor store key dir initState curCode ()

data TransactionConfig t a = TransactionConfig
  { _transCfg_scopes  :: [Text]
  , _transCfg_mode    :: DOM.IDBTransactionMode
  , _transCfg_trigger :: Event t a
  , _transCfg_abort   :: Event t ()
  }

nullErr :: String -> [String] -> Text
nullErr op args = T.pack $ op ++ " returned null, when called with args" ++ (show args)

runTransaction :: forall t m input output.
  ( MonadIO m
  , MonadSample t m
  , MonadWidget t m
  ) => IndexedDB t
    -> TransactionConfig t input
    -> (forall t1. (Reflex t1) => IDB t1 input (WidgetHost m) output)
    -> m (Event t (Either IDBError output))
runTransaction idb transCfg transact = do
  let trigger = _transCfg_trigger transCfg
      fixT = const :: IDB t input m1 o -> Event t a -> IDB t input m1 o
      code = transact `fixT` trigger
      scopes = case _transCfg_scopes transCfg of
        (s : _) -> s
        []      -> error $ "At least one trasanction scope is required"
  performEvent $ ffor trigger $ \input -> do
    idbState <- sample $ current $ _idb_state idb
    case idbState of
      Close -> return $ Left $ T.pack "Closed"
      Open idbRef' -> do
        let idbRef = runIDBRef idbRef'
        idb <- liftIO $ readIORef idbRef
        trans <- IDBD.transaction idb (T.unpack <$> _transCfg_scopes transCfg) (Just $ _transCfg_mode transCfg)
        runReaderT (runExceptT $ iterT (interpret input trans) $ runIDB code) input

interpret
  :: DOM.MonadJSM m
  => input
  -> DOM.IDBTransaction
  -> StoreOp t Item m (ExceptT IDBError (ReaderT input m) output)
  -> ExceptT IDBError (ReaderT input m) output
interpret input idbTrans (OpOpenStore storeN f) = do
      objStore <- IDBTrans.objectStore idbTrans storeN
      f $ ObjectStore objStore
interpret input idbTrans (OpAdd (ObjectStore store) item key f) = do
      liftIO $ print $ "adding " ++ (show key)
      req <- IDBStore.add store item key
      f
interpret input idbTrans (OpClear (ObjectStore store) f) = do
      req <- IDBStore.clear store
      f
interpret input idbTrans (OpCount (ObjectStore store) key f) = do
      req <- case key of
        Nothing -> do
          IDBStore.countRange store Nothing
        Just (Left keyName) -> do
          keyVal <- DOM.liftJSM $ DOM.toJSVal keyName
          IDBStore.count store keyVal
        Just (Right krange) -> do
          case krange of
            UpperBound key' b -> do
              range <- IDBKeyRan.upperBound key' b
              IDBStore.countRange store (Just range)
            LowerBound key' b -> do
              range <- IDBKeyRan.lowerBound key' b
              IDBStore.countRange store (Just range)
            Bound (k1, b1) (k2, b2) -> do
              range <- IDBKeyRan.bound k1 k2 b1 b2
              IDBStore.countRange store (Just range)
            Only k -> do
              range <- IDBKeyRan.only k
              IDBStore.countRange store (Just range)
      f (Left $ T.pack "TODO")
interpret input idbTrans (OpGet (ObjectStore store) key f) = do
      keyVal <- DOM.liftJSM $ DOM.toJSVal key
      req <- IDBStore.get store key
      f (Left $ T.pack "TODO")
interpret input idbTrans (OpGetAll (ObjectStore store) key kcount f) = do
  error "getAll is not available in GHCJS"
interpret input idbTrans (OpGetAllKeys (ObjectStore store) key kcount f) = do
  error "getAllKeys is not available in GHCJS"
interpret input idbTrans (OpIndex (ObjectStore store) idx f) = do
      req <- IDBStore.index store idx
      f
interpret input idbTrans (OpNewIndex (ObjectStore store) (idxName, key, idxPar_) f) = do
      idxPar' <- case idxPar_ of
        Nothing -> return Nothing
        Just idxPar ->
          fmap (Just . DOM.IDBIndexParameters) $ DOM.liftJSM $ DOM.toJSVal $ A.object
                 [ (T.pack "unique"     .= isUnique idxPar)
                 , (T.pack "multiEntry" .= isMultiEntry idxPar)
                 , (T.pack "locale"     .= locale idxPar)
                 ]
      idx <- IDBStore.createIndex store (idxName) (T.unpack key) idxPar'
      f
interpret input idbTrans (OpDelete (ObjectStore store) key f) = do
      req <- IDBStore.delete store key
      f
interpret input idbTrans (OpDeleteIndex (ObjectStore store) key f) = do
      req <- IDBStore.deleteIndex store key
      f
interpret input idbTrans (OpCursor (ObjectStore store) key' move init curOps f) = do
      req <- case key' of
        Nothing -> do
          IDBStore.openCursorRange store Nothing move
        Just (Left key) -> do
          IDBStore.openCursor store key move
        Just (Right krange) -> do
          case krange of
            UpperBound key' b -> do
              range <- IDBKeyRan.upperBound key' b
              IDBStore.openCursorRange store (Just range) move
            LowerBound key' b -> do
              range <- IDBKeyRan.lowerBound key' b
              IDBStore.openCursorRange store (Just range) move
            Bound (k1, b1) (k2, b2) -> do
              range <- IDBKeyRan.bound k1 k2 b1 b2
              IDBStore.openCursorRange store (Just range) move
            Only k -> do
              range <- IDBKeyRan.only k
              IDBStore.openCursorRange store (Just range) move
      let cursorRes = runStateT (runExceptT $ iterT (interpretCursor undefined) (runCursor curOps)) init
      f
interpret input idbTrans (OpKeyCursor (ObjectStore store) key move curOps f) = do
      req <- IDBStore.openCursorRange store Nothing move
      f
interpret input idbTrans (OpPut (ObjectStore store) item key f) = do
      req <- IDBStore.put store item (Just key)
      f

interpretCursor curs (OpContinue f) = do
      f

getInput :: Monad m => IDB t r m r
getInput = IDB ((lift . lift) ask)
