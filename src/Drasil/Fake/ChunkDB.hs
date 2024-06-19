module Drasil.Fake.ChunkDB where

import qualified Data.Map.Strict as M

type UID = String
type Chunk = M.Map String String
type ChunkDB = M.Map UID Chunk

class HasUID t where
    uid :: t -> UID
