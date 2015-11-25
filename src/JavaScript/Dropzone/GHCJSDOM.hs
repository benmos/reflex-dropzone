{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI #-}
#endif
module JavaScript.Dropzone.GHCJSDOM (
  -- * Types
  Dropzone(..),

  -- * Functions
  private_newDropzone,
#ifdef ghcjs_HOST_OS
  dropzoneRegisterListener,
  castToDropzone,
  toDropzone
#endif
)

where

import GHCJS.DOM.HTMLElement

#ifdef ghcjs_HOST_OS
import JavaScript.Dropzone.Extras
import GHCJS.DOM.Types
import GHCJS.Foreign
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types
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
-- Listener registration for JavaScript --> Haskell event communication

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

-- Function adapted from GHCJS.DOM.EventTargetClosures 'eventTargetAddEventListener'
#ifdef ghcjs_HOST_OS
dropzoneRegisterListener :: (ToJSString eventName, GObjectClass event) =>
                           Dropzone -> eventName -> (Dropzone -> event -> IO ()) -> IO (IO ())
dropzoneRegisterListener self eventName user = do
    callback <- syncCallback1 AlwaysRetain True $ \e -> user self (unsafeCastGObject $ GObject e)
    _        <- js_dropzone_on (unGObject (toGObject self)) (toJSString eventName) callback
    return $ do
        _   <- js_dropzone_off (unGObject (toGObject self)) (toJSString eventName) callback
        release callback
#endif


------------------------------------------------------------------------
-- Constructors

#ifdef ghcjs_HOST_OS
foreign import javascript safe
  "(new Dropzone($1, $2))"
  js_newDropzone_raw :: HTMLElement -> JSString -> IO (JSRef Dropzone)
js_newDropzone :: HTMLElement -> String -> IO (JSRef Dropzone)
js_newDropzone elt opts = js_newDropzone_raw elt (toJSString opts)
#else
js_newDropzone :: HTMLElement -> String -> IO ()
js_newDropzone = error "js_newDropzone is only available under GHCJS"
#endif

-- We don't expose this because...
-- ...Dropzone seems to be unhappy if it's created too early
-- eg Errors like this after adding files by 'click':
-- "null is not an object (evaluating '_this.hiddenFileInput.parentNode.removeChild')"
private_newDropzone :: HTMLElement -> String -> IO Dropzone
private_newDropzone e opts = Dropzone <$> js_newDropzone e opts
                 
