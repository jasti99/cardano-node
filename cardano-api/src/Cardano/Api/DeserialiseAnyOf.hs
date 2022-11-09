{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.DeserialiseAnyOf
  (
  ) where

import           Prelude

import           Control.Exception (Exception (..), IOException, throwIO)
import           System.IO (Handle)

