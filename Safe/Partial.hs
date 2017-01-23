{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE ImplicitParams  #-}
-- | ConstraintKind synonym for marking partial functions
module Safe.Partial(
  Partial
  ) where

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (HasCallStack)
#elif MIN_VERSION_base(4,8,1)
import GHC.Stack (CallStack)
#else
import GHC.Exts (Constraint)
#endif

-- | A constraint synonym which denotes that the function is partial, and will
--   (on GHC 8.* and up) produce a stack trace on failure.
--   You may mark your own non-total functions as Partial, if necessary, and this
--   will ensure that they produce useful stack traces.

#if MIN_VERSION_base(4,9,0)
type Partial = HasCallStack
#elif MIN_VERSION_base(4,8,1)
type Partial = (?loc :: CallStack)
#else
type Partial = (() :: Constraint)
#endif
