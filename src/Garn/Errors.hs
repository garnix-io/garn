module Garn.Errors where

import Control.Exception (Exception)

data UserError = UserError String
  deriving stock (Eq, Show)

instance Exception UserError
