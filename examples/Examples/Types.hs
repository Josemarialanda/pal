-- |
-- Module      : Examples.Types
-- Description : Shared shape of a runnable PAL example.
module Examples.Types where

import Polysemy (Member, Sem)
import Types (Err, PAL, Type)

-- | A PAL program, polymorphic in the effect stack so any interpreter can run it.
newtype Program = Program (forall r. (Member PAL r) => Sem r (Either Err Type))

-- | A named example. Loading may need IO (e.g. reading a @.pal@ file) and may
--   fail (e.g. a parse error), hence @IO (Either String Program)@.
data Example = Example
  { exName :: String,
    exDescription :: String,
    exLoad :: IO (Either String Program)
  }

-- | An example whose program is available without IO.
pureExample :: String -> String -> (forall r. (Member PAL r) => Sem r (Either Err Type)) -> Example
pureExample name description program =
  Example name description (pure (Right (Program program)))
