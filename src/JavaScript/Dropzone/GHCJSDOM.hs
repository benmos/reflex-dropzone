{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, ForeignFunctionInterface, FlexibleContexts #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI #-}
#endif
module JavaScript.Dropzone.GHCJSDOM (
  -- * Types
  Dropzone(..),

  -- * Core        
  private_newDropzone,
#ifdef ghcjs_HOST_OS
  dropzoneRegisterListener,
#endif

  -- * Extra
#ifdef ghcjs_HOST_OS
  castToDropzone,
  toDropzone
#endif
)

where

#ifdef ghcjs_HOST_OS
import JavaScript.Dropzone.Extras
#endif

import GHCJS.DOM.HTMLElement

#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Types
import GHCJS.Foreign
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types
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
dropzoneRegisterListener ::
                         (GObjectClass self, ToJSString eventName, GObjectClass event) =>
                           self -> eventName -> (self -> event -> IO ()) -> IO (IO ())
dropzoneRegisterListener self eventName user = do
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
                 
