module Drasil.Fake.Theories where

import Drasil.Fake.ChunkDB (UID, Chunk, HasUID (uid))
import qualified Data.Map.Strict as M
import Data.List (intercalate)

data Theory = Theory {
    _uid :: UID,
    title :: String,
    things :: [String]
} deriving (Eq, Ord, Show)

instance HasUID Theory where
  uid = _uid

thToChunk :: Theory -> Chunk
thToChunk th = M.fromList [("uid", uid th), ("title", title th), ("things", intercalate "/" (things th))]
