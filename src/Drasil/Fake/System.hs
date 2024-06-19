module Drasil.Fake.System where

import Drasil.Fake.Theories (Theory)
import Drasil.Fake.ChunkDB (UID, HasUID (uid))

data System = System {
    _uid :: UID,
    author :: String,
    purpose :: String,
    title :: String,
    theories :: [Theory]
} deriving (Show)

instance HasUID System where
    uid = _uid
