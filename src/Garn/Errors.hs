module Garn.Errors where

import Control.Exception (Exception)

data UserError = UserError String
  deriving stock (Show)

instance Exception UserError
