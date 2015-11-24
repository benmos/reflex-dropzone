{-# LANGUAGE CPP, RecursiveDo, Rank2Types, OverloadedStrings, ScopedTypeVariables, TupleSections, RecordWildCards, ForeignFunctionInterface, FlexibleContexts #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI #-}
#endif
module JavaScript.Dropzone (
  -- * Core        
  newDropzoneOn,
  dropzoneAddedEventForEvent,

  -- * Extra
  castToDropzone,
  castTo,
  toDropzone
)

where

import Control.Monad.IO.Class

import GHCJS.DOM.File
import GHCJS.DOM.HTMLElement
import Reflex
import Reflex.Dom

#ifdef ghcjs_HOST_OS
import Control.Monad.Trans.Reader
import GHCJS.DOM.Types hiding (Event)
import GHCJS.Foreign
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types
#endif

------------------------------------------------------------------------
-- Utility functions mostly copied from GHCJS.DOM.Types
-- ('castTo' is not exported from there)...

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "h$isInstanceOf $1 $2"
    typeInstanceIsA' :: JSRef a -> JSRef GType -> Bool

typeInstanceIsA :: JSRef a -> GType -> Bool
typeInstanceIsA o (GType t) = typeInstanceIsA' o t

castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objRef)
      | typeInstanceIsA objRef gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName
#endif


------------------------------------------------------------------------
-- Utility function copied from GHCJS.DOM.EventTargetClosures
#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
        "(console['log']($1));($1[\"on\"]($2, $3) ? 1 : 0)"
        js_dropzone_on ::
        JSRef GObject -> JSString -> JSRef a -> IO Bool

foreign import javascript unsafe
        "($1[\"off\"]($2, $3) ? 1 : 0)"
        js_dropzone_off ::
        JSRef GObject -> JSString -> JSRef a -> IO Bool
#endif

-- Utility function adapted from GHCJS.DOM.EventTargetClosures
-- (the type used there is overly restrictive and insists on 'IsEvent event')...
#ifdef ghcjs_HOST_OS
eventTargetAddEventListener ::
                         (GObjectClass self, ToJSString eventName, GObjectClass event) =>
                           self -> eventName -> (self -> event -> IO ()) -> IO (IO ())
eventTargetAddEventListener self eventName user = do
    callback <- syncCallback1 AlwaysRetain True $ \e -> user self (unsafeCastGObject $ GObject e)
    _        <- js_dropzone_on (unGObject (toGObject self)) (toJSString eventName) callback
    return $ do
        _   <- js_dropzone_off (unGObject (toGObject self)) (toJSString eventName) callback
        release callback
#endif

------------------------------------------------------------------------
-- Define our 'Dropzone' object...

#ifdef ghcjs_HOST_OS
newtype Dropzone = Dropzone (JSRef Dropzone)

unDropzone :: Dropzone -> JSRef Dropzone
unDropzone (Dropzone o) = o

instance ToJSRef Dropzone where
  toJSRef = return . unDropzone
  {-# INLINE toJSRef #-}

instance FromJSRef Dropzone where
  fromJSRef = return . fmap Dropzone . maybeJSNull
  {-# INLINE fromJSRef #-}

class GObjectClass o => IsDropzone o
toDropzone :: IsDropzone o => o -> Dropzone
toDropzone = unsafeCastGObject . toGObject

instance IsDropzone Dropzone
instance GObjectClass Dropzone where
  toGObject = GObject . castRef . unDropzone
  unsafeCastGObject = Dropzone . castRef . unGObject

castToDropzone :: GObjectClass obj => obj -> Dropzone
castToDropzone = castTo gTypeDropzone "Dropzone"

foreign import javascript unsafe "window[\"Dropzone\"]" gTypeDropzone' :: JSRef GType
gTypeDropzone :: GType
gTypeDropzone = GType gTypeDropzone'
#else
newtype Dropzone = Dropzone ()
#endif

------------------------------------------------------------------------
-- Now we /use/ our Dropzone object 

#ifdef ghcjs_HOST_OS
foreign import javascript safe
  "(new Dropzone($1, { autoProcessQueue: false, addRemoveLinks: true, url: \"/file/post\"}))"
  js_newDropzone :: HTMLElement -> IO (JSRef Dropzone)
#else
js_newDropzone :: HTMLElement -> IO ()
js_newDropzone = error "js_newDropzone is only available under GHCJS"
#endif

-- We don't expose this because...
-- ...Dropzone seems to be unhappy if it's created too early
-- eg Errors like this after adding files by 'click':
-- "null is not an object (evaluating '_this.hiddenFileInput.parentNode.removeChild')"
private_newDropzone :: HTMLElement -> IO Dropzone
private_newDropzone e = Dropzone <$> js_newDropzone e
                 
-- | Dropzone seems to be unhappy if it's created too early
--   eg Errors like this after adding files by 'click':
-- >
-- > "null is not an object (evaluating '_this.hiddenFileInput.parentNode.removeChild')"
-- >
-- ... so instead of exposing 'private_newDropzone' directly, we expose
-- a Reflex 'Event' which will supply the 'Dropzone' once it has been
-- safely constructed.
newDropzoneOn :: MonadWidget t m => HTMLElement -> m (Event t Dropzone)
newDropzoneOn elt = do
  pb       <- getPostBuild
  edz      <- performEvent $ (liftIO $ private_newDropzone elt) <$ pb
  return edz

------------------------------------------------------------------------
-- Reflex Events

dropzoneAddedEventForEvent :: forall t m . MonadWidget t m => Event t Dropzone -> m (Event t File)
dropzoneAddedEventForEvent edropzone = do
  let eefile = dropzoneAddedEvent <$> edropzone ::   Event t (m (Event t File))
  ddzadd <- widgetHold (return never) eefile    :: m (Dynamic t (Event t File))
  return $ switchPromptlyDyn ddzadd             ::            m (Event t File)

#ifdef ghcjs_HOST_OS
dropzoneAddedEvent :: forall t m . MonadWidget t m => Dropzone -> m (Event t File)
dropzoneAddedEvent elt = wrapDomEvent elt (connect "addedfile") (asks snd) -- BM: "asks snd" == return the File as the value of the Reflex Event
  where
    -- connect :: (GObjectClass t, IsEvent e) => String -> t -> ReaderT (t,e) IO () -> IO (IO ())
    connect :: String -> Dropzone -> ReaderT (Dropzone, File) IO () -> IO (IO ())
    connect eventName target callback =
      eventTargetAddEventListener target eventName $ curry (runReaderT callback)

#else
dropzoneAddedEvent :: MonadWidget t m => Dropzone -> m (Event t File)
dropzoneAddedEvent = const $ return never
#endif

