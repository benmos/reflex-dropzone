{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
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
  -- dropzoneRegisterListenerObj,
  -- dropzoneRegisterListenerObjRef
#endif
)

where

import GHCJS.DOM.HTMLElement

import qualified Data.Aeson as Aeson

#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..)) -- 2015-10-06 Rename JSRef to JSVal...
import GHCJS.Types
#endif

------------------------------------------------------------------------
-- Define our 'Dropzone' object...

#ifdef ghcjs_HOST_OS
newtype Dropzone = Dropzone { unDropzone :: JSVal }

instance Eq (Dropzone) where
  (Dropzone a) == (Dropzone b) = js_eq a b -- js_eq [GHCJS.DOM.Types:886]

instance PToJSVal Dropzone where
  pToJSVal = unDropzone
  {-# INLINE pToJSVal #-}

instance PFromJSVal Dropzone where
  pFromJSVal = Dropzone
  {-# INLINE pFromJSVal #-}

instance ToJSVal Dropzone where
  toJSVal = return . unDropzone
  {-# INLINE toJSVal #-}

instance FromJSVal Dropzone where
  fromJSVal = return . fmap Dropzone . maybeJSNullOrUndefined -- maybeJSNullOrUndefined [GHCJS.DOM.Types:878]
  {-# INLINE fromJSVal #-}

#else
newtype Dropzone = Dropzone ()
#endif


------------------------------------------------------------------------
-- Constructors

#ifdef ghcjs_HOST_OS
foreign import javascript safe
  "(new Dropzone($1, $2))"
  js_newDropzone_raw :: HTMLElement -> JSVal -> IO JSVal
js_newDropzone :: HTMLElement -> Aeson.Value -> IO JSVal
-- js_newDropzone elt opts = js_newDropzone_raw elt =<< toJSRef opts
js_newDropzone elt opts = js_newDropzone_raw elt =<< toJSVal opts
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

-- OLD EXAMPLE CODE:
-- foreign import javascript unsafe
--         "($1[\"addEventListener\"]($2, $3, $4) ? 1 : 0)"
--         ghcjs_dom_event_target_add_event_listener ::
--         JSRef GObject -> JSString -> JSRef a -> Bool -> IO Bool

-- EXAMPLE CODE:
-- foreign import javascript unsafe
--         "$1[\"addEventListener\"]($2, $3,\n$4)" js_addEventListener ::
--         EventTarget -> JSString -> Nullable EventListener -> Bool -> IO ()
--
-- addEventListener :: (MonadIO m, IsEventTarget self, ToJSString type') =>
--                    self -> type' -> Maybe EventListener -> Bool -> m ()
-- addEventListener self type' listener useCapture
--   = liftIO
--       (js_addEventListener (toEventTarget self) (toJSString type')
--          (maybeToNullable listener)
--          useCapture)

#ifdef ghcjs_HOST_OS
-- foreign import javascript unsafe
--         "($1[\"on\"]($2, $3) ? 1 : 0)" -- "(console['log']($1));($1[\"on\"]($2, $3) ? 1 : 0)"
--         js_dropzone_on_raw ::
--         JSRef -> JSString -> JSRef -> IO Bool

foreign import javascript unsafe
        "$1[\"on\"]($2, $3)"
        js_dropzone_on ::
        -- EventTarget -> JSString -> Nullable EventListener -> IO ()
        JSVal -> JSString -> Nullable JSVal -> IO ()

foreign import javascript unsafe
        "$1[\"off\"]($2, $3)"
        js_dropzone_off ::
        JSVal -> JSString -> Nullable JSVal -> IO ()
#endif

------------------------------------------------------------------------
-- Wrappers for different callback arities

-- EXAMPLE CODE:
-- ghcjs:
---------
-- jsval :: IsJSVal a => a -> JSVal                                            -- GHCJS.Internal.Types
-- jsval = jsval_                                                              -- GHCJS.Internal.Types
-- newtype Callback a = Callback JSVal deriving Typeable                       -- GHCJS.Foreign.Callback.Internal
-- instance IsJSVal (Callback a)                                               -- GHCJS.Foreign.Callback.Internal
--
-- syncCallback1 :: OnBlocked                             -- ^ what to do when the thread blocks  -- GHCJS.Foreign.Callback
--               -> (JSVal -> IO ())                      -- ^ the Haskell function
--               -> IO (Callback (JSVal -> IO ()))        -- ^ the callback
-- syncCallback1 onBlocked x = js_syncCallbackApply (onBlocked == ContinueAsync) 1 (unsafeCoerce x)
--
--
-- ghcjs-dom:
-------------
-- type    DOMString              = JSString                                   -- GHCJS.DOM.EventM
-- type    EventM             t e = ReaderT e IO                               -- GHCJS.DOM.EventM
-- newtype EventListener          = EventListener { unEventListener :: JSVal } -- GHCJS.DOM.Types
-- newtype EventTarget            = EventTarget   { unEventTarget   :: JSVal } -- GHCJS.DOM.Types
-- newtype EventName          t e = EventName          DOMString               -- GHCJS.DOM.EventTargetClosures
-- newtype SaferEventListener t e = SaferEventListener EventListener           -- GHCJS.DOM.EventTargetClosures
--
-- foreign import javascript unsafe                                            -- GHCJS.DOM.Types
--   "$1===$2" js_eq :: JSVal -> JSVal -> Bool
--
-- maybeJSNullOrUndefined :: JSVal -> Maybe JSVal
-- maybeJSNullOrUndefined r | isNull r || isUndefined r = Nothing              -- GHCJS.DOM.Types
-- maybeJSNullOrUndefined r = Just r
--
-- on :: (IsEventTarget t, IsEvent e) => t -> EventName t e -> EventM t e   () -> IO (IO ()) -- GHCJS.DOM.EventM
-- on :: (IsEventTarget t, IsEvent e) => t -> EventName t e -> ReaderT e IO () -> IO (IO ())
-- on target eventName callback = do
--     l <- newListener callback
--     addListener target eventName l False
--     return (removeListener target eventName l False)
--
-- newListener :: (IsEvent e) => EventM t e () -> IO (SaferEventListener t e)                -- GHCJS.DOM.EventM
-- newListener f = SaferEventListener <$> eventListenerNew (runReaderT f)
--
-- eventListenerNew :: IsEvent event => (event -> IO ()) -> IO EventListener                 -- GHCJS.DOM.EventTargetClosures
-- eventListenerNew callback = (EventListener . jsval) <$> syncCallback1 ContinueAsync (callback . unsafeCastGObject . GObject)

------------------------------------------------------------------------
-- newListenerDZ :: EventM Dropzone e () -> IO (SaferEventListener Dropzone e) -- GHCJS.DOM.EventM
-- newListenerDZ f = SaferEventListener <$> eventListenerNewDZ (runReaderT f)
-- newListenerDZ :: IO () -> IO (SaferEventListener Dropzone e) -- GHCJS.DOM.EventM
-- newListenerDZ f = SaferEventListener <$> eventListenerNewDZ f

-- eventListenerNewDZ :: IO () -> IO EventListener                             -- GHCJS.DOM.EventTargetClosures
-- eventListenerNewDZ callback = (EventListener . jsval) <$> syncCallback ContinueAsync callback

-- releaseListenerDZ :: SaferEventListener Dropzone e -> IO ()
-- releaseListenerDZ (SaferEventListener l) = eventListenerReleaseDZ l

-- eventListenerReleaseDZ :: EventListener -> IO ()
-- eventListenerReleaseDZ (EventListener ref) = releaseCallback (Callback ref)

------------------------------------------------------------------------

#ifdef ghcjs_HOST_OS
-- Function originally adapted from GHCJS.DOM.EventTargetClosures 'eventTargetAddEventListener' (old-base)
-- and GHCJS.DOM.EventM 'on' (improved-base)
dropzoneRegisterListener0 :: ToJSString e => Dropzone -> e -> (Dropzone -> IO ()) -> IO (IO ())
dropzoneRegisterListener0 dz eventName usercb = do
    -- callback <- syncCallback AlwaysRetain True $ callback dz
    callback <- syncCallback ContinueAsync $ usercb dz
    _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
    return $ do
        _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
        releaseCallback callback
    -- callback <- newListenerDZ $ (ReaderT (const $ usercb dz) :: EventM Dropzone e ())
    -- let e :: EventName Dropzone e
    --     e = EventName $ toJSString eventName
    --     el :: SaferEventListener Dropzone e
    --     el = SaferEventListener . EventListener . jsval $ callback
    -- _        <- dropzone_on dz e el
    -- _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
    -- return $ do
        -- _   <- dropzone_off dz e el
        -- _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
        -- releaseCallback callback
#endif

-- Function originally adapted from GHCJS.DOM.EventTargetClosures 'eventTargetAddEventListener'
#ifdef ghcjs_HOST_OS
dropzoneRegisterListener1 :: ToJSString e => Dropzone -> e -> (Dropzone -> JSVal -> IO ()) -> IO (IO ())
dropzoneRegisterListener1 dz eventName usercb = do
    callback <- syncCallback1 ContinueAsync $ \a -> usercb dz a
    _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
    return $ do
        _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
        releaseCallback callback
#endif

#ifdef ghcjs_HOST_OS
dropzoneRegisterListener2 :: (ToJSString eventName) =>
                             Dropzone -> eventName -> (Dropzone -> JSVal -> JSVal -> IO ()) -> IO (IO ())
dropzoneRegisterListener2 dz eventName user = do
    callback <- syncCallback2 ContinueAsync $ \a b -> user dz a b
    _        <- js_dropzone_on (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
    return $ do
        _   <- js_dropzone_off (unDropzone dz) (toJSString eventName) (maybeToNullable $ Just $ jsval callback)
        releaseCallback callback
#endif
                 



------------------------------------------------------------------------
-- (optional) ghcjs-dom interoperability
-- "optional" here means that everything should compile with this commented-out

#ifdef ghcjs_HOST_OS
-- Utility functions mostly copied from GHCJS.DOM.Types
-- ('castTo' is not exported from there)...

foreign import javascript unsafe "h$isInstanceOf $1 $2"
    typeInstanceIsA' :: JSVal -> JSVal -> Bool

typeInstanceIsA :: JSVal -> GType -> Bool
typeInstanceIsA o (GType t) = typeInstanceIsA' o t

castTo :: (IsGObject obj, IsGObject obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objRef)
      | typeInstanceIsA objRef gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName

#endif


#ifdef ghcjs_HOST_OS
class IsEventTarget o => IsDropzone o
toDropzone :: IsDropzone o => o -> Dropzone
toDropzone = unsafeCastGObject . toGObject

instance IsDropzone Dropzone
instance IsEventTarget Dropzone

instance IsGObject Dropzone where
  toGObject = GObject . unDropzone
  {-# INLINE toGObject #-}
  unsafeCastGObject = Dropzone . unGObject
  {-# INLINE unsafeCastGObject #-}

castToDropzone :: IsGObject obj => obj -> Dropzone
castToDropzone = castTo gTypeDropzone "Dropzone"

foreign import javascript unsafe "window[\"Dropzone\"]" gTypeDropzone :: GType
#endif

-- Convenience wrappers for 'ghcjs-dom' types
-- (ie Wrapping the first arg as a 'GObjectClass a =>' - eg a 'File' or 'Event')

{-
#ifdef ghcjs_HOST_OS
dropzoneRegisterListenerObj :: (ToJSString eventName, GObjectClass a) =>
                               Dropzone -> eventName -> (Dropzone -> a -> IO ()) -> IO (IO ())
dropzoneRegisterListenerObj self eventName user = dropzoneRegisterListener1 self eventName $
                                                  \dz a -> user dz (unsafeCastGObject $ GObject a)
#endif

#ifdef ghcjs_HOST_OS
dropzoneRegisterListenerObjRef :: (ToJSString eventName, GObjectClass a) =>
                                  Dropzone -> eventName -> (Dropzone -> a -> JSVal b -> IO ()) -> IO (IO ())
dropzoneRegisterListenerObjRef self eventName user = dropzoneRegisterListener2 self eventName $
                                                  \dz a b -> user dz (unsafeCastGObject $ GObject a) b
#endif
-}
------------------------------------------------------------------------

