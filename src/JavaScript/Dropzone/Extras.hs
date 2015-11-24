{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, ForeignFunctionInterface, JavaScriptFFI #-}
module JavaScript.Dropzone.Extras (
#ifdef ghcjs_HOST_OS
  castTo
#endif
)

where

#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Types
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

