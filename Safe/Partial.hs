{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE ImplicitParams  #-}
-- | ConstraintKind synonym for marking partial functions
module Safe.Partial(
  Partial
  ) where

#if __GLASGOW_HASKELL__ < 710
import GHC.Exts (Constraint)
#elif __GLASGOW_HASKELL__ < 800
import GHC.Stack (CallStack)
#else
import GHC.Stack (HasCallStack)
#endif

-- | A constraint synonym which denotes that the function is partial, and will
--   (on GHC 8.* and up) produce a stack trace on failure.
--   You may mark your own non-total functions as Partial, if necessary, and this
--   will ensure that they produce useful stack traces.
#if __GLASGOW_HASKELL__ < 710
type Partial = (() :: Constraint)
#elif __GLASGOW_HASKELL__ < 800
type Partial = (?loc :: CallStack)
#else
type Partial = HasCallStack
#endif
