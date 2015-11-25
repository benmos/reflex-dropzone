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
  dropzoneAddedFile
)

where

import JavaScript.Dropzone.Core

import Control.Arrow ((***))
import Control.Monad.IO.Class
import Data.String
import GHCJS.DOM.File
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Event) -- Clashes with Reflex

import Reflex
import Reflex.Dom

import qualified Data.Text       as T
import qualified GHCJS.DOM.Types as GJST

#ifdef ghcjs_HOST_OS
import Control.Monad.Trans.Reader
import GHCJS.Foreign
import GHCJS.Types
#endif

reflexDropzoneOptions :: IsString a => a
reflexDropzoneOptions = "{ autoProcessQueue: false, addRemoveLinks: true, url: \"/file/post\"}"

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
newDropzoneOn :: MonadWidget t m => HTMLElement -> String -> m (Event t Dropzone)
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
dropzoneEventFor1 :: forall t m a . (MonadWidget t m) => String -> Dropzone -> m (Event t (JSRef a))
dropzoneEventFor1 ename elt = wrapDomEvent elt (connect ename) (asks snd) -- "asks snd": use the obj as value of Reflex Event
  where
    connect :: String -> Dropzone -> ReaderT (Dropzone, (JSRef a)) IO () -> IO (IO ())
    connect eventName target callback =
      dropzoneRegisterListener1 target eventName $ \dz a -> runReaderT callback (dz, a)
#else
-- | Callbacks with arity 1
dropzoneEventFor1 :: forall t m a . (MonadWidget t m) => String -> Dropzone -> m (Event t (JSRef a))
dropzoneEventFor1 _ _ = return never
#endif


#ifdef ghcjs_HOST_OS
-- | Callbacks with arity 2
dropzoneEventFor2 :: forall t m a b . (MonadWidget t m) => String -> Dropzone -> m (Event t (JSRef a, JSRef b))
dropzoneEventFor2 ename elt = wrapDomEvent elt (connect ename) (asks snd) -- "asks snd": use the objs as value of Reflex Event
  where
    connect :: String -> Dropzone -> ReaderT (Dropzone, (JSRef a, JSRef b)) IO () -> IO (IO ())
    connect eventName target callback =
      dropzoneRegisterListener2 target eventName $ \dz a b -> runReaderT callback (dz, (a, b))
#else
-- | Callbacks with arity 2
dropzoneEventFor2 :: forall t m a b . (MonadWidget t m) => String -> Dropzone -> m (Event t (JSRef a, JSRef b))
dropzoneEventFor2 _ _ = return never
#endif


------------------------------------------------------------------------
-- Reflex Events - Different Types

dropzoneEventForObj :: forall t m a . (MonadWidget t m, GObjectClass a) => String -> Dropzone -> m (Event t a)
dropzoneEventForObj self eventName = fmap (unsafeCastGObject . GObject) <$> dropzoneEventFor1 self eventName

dropzoneEventForFile :: MonadWidget t m => String -> Dropzone -> m (Event t File)
dropzoneEventForFile = dropzoneEventForObj

dropzoneEventForDOMEvent :: MonadWidget t m => String -> Dropzone -> m (Event t GJST.Event)
dropzoneEventForDOMEvent = dropzoneEventForObj

#ifdef ghcjs_HOST_OS
dropzoneEventForFileText :: MonadWidget t m => String -> Dropzone -> m (Event t (File, T.Text))
dropzoneEventForFileText ename dz = fmap ((unsafeCastGObject . GObject) *** fromJSString) <$> dropzoneEventFor2 ename dz
#else
dropzoneEventForFileText :: MonadWidget t m => String -> Dropzone -> m (Event t (File, T.Text))
dropzoneEventForFileText _ _ = return never
#endif


------------------------------------------------------------------------
-- Specific Dropzone Events

-- | Register for "addedfile" events on the Dropzone
dropzoneAddedFile :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneAddedFile = dropzoneEventForFile "addedfile"

