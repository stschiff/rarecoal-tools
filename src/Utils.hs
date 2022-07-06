module Utils where

import Control.Exception (Exception)

data PopNotFoundException = PopNotFoundException String deriving Show
instance Exception PopNotFoundException
