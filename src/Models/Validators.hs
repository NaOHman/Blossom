module Models.Validators 
    ( module Models.Core
    , Validator
    ) where 

import Models.Core
import Control.Monad.Trans.Either
import Control.Monad.State

type Validator s a = EitherT String (State s) a
