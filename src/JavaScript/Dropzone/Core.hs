{-# LANGUAGE CPP, OverloadedStrings #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif
module JavaScript.Dropzone.Core (
  -- * Types
  Dropzone(..),

  -- * Functions
  private_newDropzone,
#ifdef ghcjs_HOST_OS
  dropzoneRegisterListener0,
  dropzoneRegisterListener1,
  dropzoneRegisterListener2,

  -- * (optional) ghcjs-dom interoperability
  castToDropzone,
  toDropzone,
  dropzoneRegisterListenerObj,
  dropzoneRegisterListenerObjRef
#endif
)

where

import GHCJS.DOM.HTMLElement

import qualified Data.Aeson as Aeson

#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Types
import GHCJS.Foreign
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))
import GHCJS.Types
#endif

------------------------------------------------------------------------
-- Define our 'Dropzone' object...

#ifdef ghcjs_HOST_OS
newtype Dropzone = Dropzone { unDropzone :: JSRef Dropzone }

instance ToJSRef Dropzone where
  toJSRef = return . unDropzone
  {-# INLINE toJSRef #-}

instance FromJSRef Dropzone where
  fromJSRef = return . fmap Dropzone . maybeJSNull
  {-# INLINE fromJSRef #-}
#else
newtype Dropzone = Dropzone ()
#endif


------------------------------------------------------------------------
-- Constructors

#ifdef ghcjs_HOST_OS
foreign import javascript safe
  "(new Dropzone($1, $2))"
  js_newDropzone_raw :: HTMLElement -> JSRef Aeson.Value  -> IO (JSRef Dropzone)
js_newDropzone :: HTMLElement -> Aeson.Value -> IO (JSRef Dropzone)
js_newDropzone elt opts = js_newDropzone_raw elt =<< toJSRef opts
#else
js_newDropzone :: HTMLElement -> Aeson.Value -> IO ()
js_newDropzone = error "js_newDropzone is only available under GHCJS"
#endif

-- We don't expose this because...
-- ...Dropzone seems to be unhappy if it's created too early
-- eg Errors like this after adding files by 'click':
-- "null is not an object (evaluating '_this.hiddenFileInput.parentNode.removeChild')"
private_newDropzone :: HTMLElement -> Aeson.Value -> IO Dropzone
private_newDropzone e opts = Dropzone <$> js_newDropzone e opts


------------------------------------------------------------------------
-- Listener registration for JavaScript --> Haskell event communication

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
        "($1[\"on\"]($2, $3) ? 1 : 0)" -- "(console['log']($1));($1[\"on\"]($2, $3) ? 1 : 0)"
        js_dropzone_on ::
        JSRef Dropzone -> JSString -> JSRef a -> IO Bool

foreign import javascript unsafe
        "($1[\"off\"]($2, $3) ? 1 : 0)"
        js_dropzone_off ::
        JSRef Dropzone -> JSString -> JSRef a -> IO Bool
#endif


------------------------------------------------------------------------
-- Wrappers for different callback arities

#ifdef ghcjs_HOST_OS
dropzoneRegisterListener0 :: ToJSString e => Dropzone -> e -> (Dropzone -> IO ()) -> IO (IO ())
dropzoneRegisterListener0 dz eventName user = do
    callback <- syncCallback AlwaysRetain True $ user dz
    _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) callback
    return $ do
        _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) callback
        release callback
#endif

-- Function originally adapted from GHCJS.DOM.EventTargetClosures 'eventTargetAddEventListener'
#ifdef ghcjs_HOST_OS
dropzoneRegisterListener1 :: ToJSString e => Dropzone -> e -> (Dropzone -> JSRef a -> IO ()) -> IO (IO ())
dropzoneRegisterListener1 dz eventName user = do
    callback <- syncCallback1 AlwaysRetain True $ \a -> user dz a
    _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) callback
    return $ do
        _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) callback
        release callback
#endif

#ifdef ghcjs_HOST_OS
dropzoneRegisterListener2 :: (ToJSString eventName) =>
                             Dropzone -> eventName -> (Dropzone -> JSRef a -> JSRef b -> IO ()) -> IO (IO ())
dropzoneRegisterListener2 dz eventName user = do
    callback <- syncCallback2 AlwaysRetain True $ \a b -> user dz a b
    _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) callback
    return $ do
        _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) callback
        release callback
#endif
                 



------------------------------------------------------------------------
-- (optional) ghcjs-dom interoperability
-- "optional" here means that everything should compile with this commented-out

#ifdef ghcjs_HOST_OS
-- Utility functions mostly copied from GHCJS.DOM.Types
-- ('castTo' is not exported from there)...
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


#ifdef ghcjs_HOST_OS
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
#endif

-- Convenience wrappers for 'ghcjs-dom' types
-- (ie Wrapping the first arg as a 'GObjectClass a =>' - eg a 'File' or 'Event')

#ifdef ghcjs_HOST_OS
dropzoneRegisterListenerObj :: (ToJSString eventName, GObjectClass a) =>
                               Dropzone -> eventName -> (Dropzone -> a -> IO ()) -> IO (IO ())
dropzoneRegisterListenerObj self eventName user = dropzoneRegisterListener1 self eventName $
                                                  \dz a -> user dz (unsafeCastGObject $ GObject a)
#endif

#ifdef ghcjs_HOST_OS
dropzoneRegisterListenerObjRef :: (ToJSString eventName, GObjectClass a) =>
                                  Dropzone -> eventName -> (Dropzone -> a -> JSRef b -> IO ()) -> IO (IO ())
dropzoneRegisterListenerObjRef self eventName user = dropzoneRegisterListener2 self eventName $
                                                  \dz a b -> user dz (unsafeCastGObject $ GObject a) b
#endif

------------------------------------------------------------------------

