module Types where

import Professor.ProfFile
--import Utils

data Bar = Bar
    { barCurrentTime  :: !Seconds
    , barTotalTime    :: !Seconds
    , barCurrentAlloc :: !Bytes
    , barTotalAlloc   :: !Bytes
    , barLabel        :: String
    }
    deriving (Show,Eq)

data CostMode = Inherited | Individual
    deriving (Show,Eq)

