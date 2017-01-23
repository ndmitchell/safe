{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP             #-}
-- | Internal ConstraintKind synonym for marking partial functions
module Safe.Partial(
  Partial
  ) where

#if MIN_VERSION_base(4,9,0)
import GHC.Stack.Types (HasCallStack)
#endif
import GHC.Exts (Constraint)

-- | A constraint synonym which denotes that the function is partial, and will
--   (on GHC 8.* and up) produce a stack trace on failure.
--   You may mark your own non-total functions as Partial if necessary, and this,
--   and this will ensure that they produce useful stack traces.
#if MIN_VERSION_base(4,9,0)
type Partial = HasCallStack
#else
type Partial = (() :: Constraint)
#endif
