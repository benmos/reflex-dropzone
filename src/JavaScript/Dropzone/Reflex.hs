{-# LANGUAGE CPP, RecursiveDo, OverloadedStrings, ScopedTypeVariables #-}
module JavaScript.Dropzone.Reflex (
  reflexDropzoneOptions,

  -- * Functions
  newDropzoneOn,
  dropzoneAddedEventForEvent
)

where

import JavaScript.Dropzone.GHCJSDOM

import Control.Monad.IO.Class
import Data.String
import GHCJS.DOM.File
import GHCJS.DOM.HTMLElement

import Reflex
import Reflex.Dom

#ifdef ghcjs_HOST_OS
import Control.Monad.Trans.Reader
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
newDropzoneOn :: MonadWidget t m => HTMLElement -> String -> m (Event t Dropzone)
newDropzoneOn elt opts = do
  pb       <- getPostBuild
  edz      <- performEvent $ (liftIO $ private_newDropzone elt opts) <$ pb
  return edz

------------------------------------------------------------------------
-- Reflex Events

-- | Variant of 'dropzoneAddedEvent' which takes an Event
dropzoneAddedEventForEvent :: forall t m . MonadWidget t m => Event t Dropzone -> m (Event t File)
dropzoneAddedEventForEvent edropzone = do
  let eefile = dropzoneAddedEvent <$> edropzone ::   Event t (m (Event t File))
  ddzadd <- widgetHold (return never) eefile    :: m (Dynamic t (Event t File))
  return $ switchPromptlyDyn ddzadd             ::            m (Event t File)

#ifdef ghcjs_HOST_OS
-- | Register for "addedfile" events on the Dropzone
dropzoneAddedEvent :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneAddedEvent elt = wrapDomEvent elt (connect "addedfile") (asks snd) -- "asks snd": use the File as value of Reflex Event
  where
    -- connect :: (GObjectClass t, IsEvent e) => String -> t -> ReaderT (t,e) IO () -> IO (IO ())
    connect :: String -> Dropzone -> ReaderT (Dropzone, File) IO () -> IO (IO ())
    connect eventName target callback =
      dropzoneRegisterListener target eventName $ curry (runReaderT callback)

#else
dropzoneAddedEvent :: MonadWidget t m => Dropzone -> m (Event t File)
dropzoneAddedEvent = const $ return never
#endif


