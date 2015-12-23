{-# LANGUAGE CPP, RecursiveDo, OverloadedStrings, ScopedTypeVariables #-}
module JavaScript.Dropzone.Reflex (
  reflexDropzoneOptions,

  -- * Functions
  newDropzoneOn,
  eventWhenReady,

  -- * Events with different callback arities
  dropzoneEventFor0,
  dropzoneEventFor1,
  dropzoneEventFor2,

  -- * Events with different callback Types
  dropzoneEventForObj,
  dropzoneEventForFile,
  dropzoneEventForDOMEvent,
  dropzoneEventForFileText,

  -- * Specific Events
  dropzoneAddedFile,
  dropzoneCancelled,
  dropzoneComplete,
  dropzoneMaxFilesExceeded,
  dropzoneProcessing,
  dropzoneRemovedFile,

  dropzoneDragEnd,
  dropzoneDragEnter,
  dropzoneDragLeave,
  dropzoneDragOver,
  dropzoneDragStart,
  dropzoneDrop,
  dropzonePaste,

  dropzoneEventForFiles,
  dropzoneError,
  dropzoneSuccess,
  dropzoneThumbnail,
  dropzoneAddedFiles,
  dropzoneCanceledMultiple,
  dropzoneCompleteMultiple,
  dropzoneProcessingMultiple,
  dropzoneQueueComplete,
  dropzoneReset
)

where

import JavaScript.Dropzone.Core

import Control.Monad.IO.Class
import Data.Aeson ((.=))
import Data.JSString.Text(textFromJSVal)
import GHCJS.DOM.File
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Event) -- Clashes with Reflex

import Reflex
import Reflex.Dom

import qualified Data.Aeson       as Aeson
import qualified Data.Text        as T
import qualified GHCJS.DOM.Types  as GJST

-- import Data.JSString.Internal.Type (JSString(..)) -- FIXME - remove dependency on this!!!!!!!!

#ifdef ghcjs_HOST_OS
import Control.Arrow ((***))
import Control.Monad.Trans.Reader
import GHCJS.Marshal
import GHCJS.Types
#endif

-- "{ autoProcessQueue: false, addRemoveLinks: true, url: \"/file/post\"}"
reflexDropzoneOptions :: Aeson.Value
reflexDropzoneOptions = Aeson.object [
  "autoProcessQueue" .= False,
  "addRemoveLinks" .= True,
  "url" .= Aeson.String "/file/post"
   ]


-- | Dropzone seems to be unhappy if it's created too early
--   eg Errors like this after adding files by 'click':
-- >
-- > "null is not an object (evaluating '_this.hiddenFileInput.parentNode.removeChild')"
-- >
-- ... so instead of exposing 'private_newDropzone' directly, we expose
-- a Reflex 'Event' which will supply the 'Dropzone' once it has been
-- safely constructed.
-- Sample usage:
--
-- > count <=< eventWhenReady dropzoneAddedFile <=< newDropzoneOn dze $ opts
--
newDropzoneOn :: MonadWidget t m => HTMLElement -> Aeson.Value -> m (Event t Dropzone)
newDropzoneOn elt opts = do
  pb       <- getPostBuild
  edz      <- performEvent $ (liftIO $ private_newDropzone elt opts) <$ pb
  return edz


-- | Delayed-initialization wrapper
--   Dropzone doesn't like being initialized too early.
--   This helper function can 'lift' event constructors to build their
--   events from an 'Event t Dropzone' rather than from a plain 'Dropzone'.
--   This allows them to be driven off the Reflex 'post build' event - eg by 'newDropzoneOn'
eventWhenReady :: forall t m dz e . MonadWidget t m => (dz -> m (Event t e)) -> Event t dz -> m (Event t e)
eventWhenReady ctr edropzone = do
  let eefile = ctr <$> edropzone                ::   Event t (m (Event t e))
  ddzadd <- widgetHold (return never) eefile    :: m (Dynamic t (Event t e))
  return $ switchPromptlyDyn ddzadd             ::            m (Event t e)


------------------------------------------------------------------------
-- Reflex Events - Different Arities
-- Register for File events on the Dropzone

#ifdef ghcjs_HOST_OS
-- | Callbacks with arity 0
dropzoneEventFor0 :: forall t m . (MonadWidget t m) => String -> Dropzone -> m (Event t ())
dropzoneEventFor0 ename elt = wrapDomEvent elt (connect ename) (return ())
  where
    connect :: String -> Dropzone -> ReaderT (Dropzone, ()) IO () -> IO (IO ())
    connect eventName target callback =
      dropzoneRegisterListener0 target eventName $ \dz -> runReaderT callback (dz, ())
#else
-- | Callbacks with arity 0
dropzoneEventFor0 :: forall t m . (MonadWidget t m) => String -> Dropzone -> m (Event t ())
dropzoneEventFor0 _ _ = return never
#endif


#ifdef ghcjs_HOST_OS
-- | Callbacks with arity 1
dropzoneEventFor1 :: forall t m . (MonadWidget t m) => String -> Dropzone -> m (Event t JSVal)
dropzoneEventFor1 ename elt = wrapDomEvent elt (connect ename) (asks snd) -- "asks snd": use the obj as value of Reflex Event
  where
    connect :: String -> Dropzone -> ReaderT (Dropzone, JSVal) IO () -> IO (IO ())
    connect eventName target callback =
      dropzoneRegisterListener1 target eventName $ \dz a -> runReaderT callback (dz, a)
#else
-- | Callbacks with arity 1
dropzoneEventFor1 :: forall t m a . (MonadWidget t m) => String -> Dropzone -> m (Event t a)
dropzoneEventFor1 _ _ = return never
#endif


#ifdef ghcjs_HOST_OS
-- | Callbacks with arity 2
dropzoneEventFor2 :: forall t m . (MonadWidget t m) => String -> Dropzone -> m (Event t (JSVal, JSVal))
dropzoneEventFor2 ename elt = wrapDomEvent elt (connect ename) (asks snd) -- "asks snd": use the objs as value of Reflex Event
  where
    connect :: String -> Dropzone -> ReaderT (Dropzone, (JSVal, JSVal)) IO () -> IO (IO ())
    connect eventName target callback =
      dropzoneRegisterListener2 target eventName $ \dz a b -> runReaderT callback (dz, (a, b))
#else
-- | Callbacks with arity 2
dropzoneEventFor2 :: forall t m a b . (MonadWidget t m) => String -> Dropzone -> m (Event t (a, b))
dropzoneEventFor2 _ _ = return never
#endif


------------------------------------------------------------------------
-- Reflex Events - Different Types

dropzoneEventForObj :: forall t m a . (MonadWidget t m, IsGObject a) => String -> Dropzone -> m (Event t a)
dropzoneEventForObj eventName self = fmap (unsafeCastGObject . GObject) <$> dropzoneEventFor1 eventName self

dropzoneEventForFile :: MonadWidget t m => String -> Dropzone -> m (Event t File)
dropzoneEventForFile = dropzoneEventForObj

dropzoneEventForDOMEvent :: MonadWidget t m => String -> Dropzone -> m (Event t GJST.Event)
dropzoneEventForDOMEvent = dropzoneEventForObj

#ifdef ghcjs_HOST_OS
dropzoneEventForFiles :: MonadWidget t m => String -> Dropzone -> m (Event t [File])
dropzoneEventForFiles eventName self = do
  eref  <- dropzoneEventFor1 eventName self
  -- erefs <- performEvent (liftIO . fromArray <$> eref) -- old-base
  erefs <- performEvent (fmap (maybe [] id) . liftIO . fromJSValListOf <$> eref)
  return $ map (unsafeCastGObject . GObject) <$> erefs
#else
dropzoneEventForFiles :: MonadWidget t m => String -> Dropzone -> m (Event t [File])
dropzoneEventForFiles  _ _ = return never
#endif

#ifdef ghcjs_HOST_OS
dropzoneEventForFileText :: MonadWidget t m => String -> Dropzone -> m (Event t (File, T.Text))
dropzoneEventForFileText ename dz = fmap ((unsafeCastGObject . GObject) *** textFromJSVal) <$> dropzoneEventFor2 ename dz
#else
dropzoneEventForFileText :: MonadWidget t m => String -> Dropzone -> m (Event t (File, T.Text))
dropzoneEventForFileText _ _ = return never
#endif


------------------------------------------------------------------------
-- Specific Dropzone Events

-- File Events

-- | Register for "addedfile" events on the Dropzone
dropzoneAddedFile :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneAddedFile = dropzoneEventForFile "addedfile"

dropzoneCancelled :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneCancelled = dropzoneEventForFile "cancelled"

dropzoneComplete :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneComplete = dropzoneEventForFile "complete"

dropzoneMaxFilesExceeded :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneMaxFilesExceeded = dropzoneEventForFile "maxfilesexceeded"

dropzoneProcessing :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneProcessing = dropzoneEventForFile "processing"

dropzoneRemovedFile :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneRemovedFile = dropzoneEventForFile "removedfile"


-- File+Text Events

dropzoneError :: forall t m . MonadWidget t m => Dropzone -> m (Event t (File, T.Text))
dropzoneError = dropzoneEventForFileText "error"

dropzoneSuccess :: forall t m . MonadWidget t m => Dropzone -> m (Event t (File, T.Text))
dropzoneSuccess = dropzoneEventForFileText "success"

dropzoneThumbnail :: forall t m . MonadWidget t m => Dropzone -> m (Event t (File, T.Text))
dropzoneThumbnail = dropzoneEventForFileText "thumbnail"


-- Multiple File Events

dropzoneAddedFiles :: forall t m . MonadWidget t m => Dropzone -> m (Event t [File])
dropzoneAddedFiles = dropzoneEventForFiles "addedfiles"

dropzoneCanceledMultiple :: forall t m . MonadWidget t m => Dropzone -> m (Event t [File])
dropzoneCanceledMultiple = dropzoneEventForFiles "canceledmultiple"

dropzoneCompleteMultiple :: forall t m . MonadWidget t m => Dropzone -> m (Event t [File])
dropzoneCompleteMultiple = dropzoneEventForFiles "completemultiple"

dropzoneProcessingMultiple :: forall t m . MonadWidget t m => Dropzone -> m (Event t [File])
dropzoneProcessingMultiple = dropzoneEventForFiles "processingmultiple"


-- DOM Events

dropzoneDragEnd :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneDragEnd = dropzoneEventForDOMEvent "dragend"

dropzoneDragEnter :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneDragEnter = dropzoneEventForDOMEvent "dragenter"

dropzoneDragLeave :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneDragLeave = dropzoneEventForDOMEvent "dragleave"

dropzoneDragOver :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneDragOver = dropzoneEventForDOMEvent "dragover"

dropzoneDragStart :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneDragStart = dropzoneEventForDOMEvent "dragstart"

dropzoneDrop :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneDrop = dropzoneEventForDOMEvent "drop"

dropzonePaste :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzonePaste = dropzoneEventForDOMEvent "paste"


-- Other
-- "sending"
-- "uploadprogress"
-- "totaluploadprogress"
-- "sendingmultiple"

dropzoneQueueComplete :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneQueueComplete = dropzoneEventForDOMEvent "queuecomplete"

dropzoneReset :: forall t m . MonadWidget t m => Dropzone -> m (Event t GJST.Event)
dropzoneReset = dropzoneEventForDOMEvent "reset"

