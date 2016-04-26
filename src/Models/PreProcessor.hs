module Models.PreProcessor where

import Control.Monad.State

type PP a = State Int a
