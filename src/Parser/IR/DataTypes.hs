module Parser.IR.DataTypes
    { Record(..)
    , Adt(..)
    }

import Parser.IR.Types

data Record =  Record
    { rType :: Type
    , fields :: [(Id, Type)]
    }
