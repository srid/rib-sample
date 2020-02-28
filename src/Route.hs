{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Route where

import Control.Monad.Catch
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Kind
import Data.Some
import Data.Text (Text)
import Path
import Rib.Target (urlForPath)

type R f = DSum f Identity

-- | Convenience builder for a 'X' using 'Identity' for the functor.
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b

{-# COMPLETE (:/) #-}

infixr 5 :/

-- | A route is a GADT representing individual routes.
--
-- The GADT type parameter represents the data used to render that particular route.
class IsRoute (r :: Type -> Type) where
  routeFile :: MonadThrow m => Some r -> m (Path Rel File)

-- | Get the URL to a route
routeUrl :: IsRoute r => Some r -> Text
routeUrl = urlForPath . either (error . displayException) id . routeFile
