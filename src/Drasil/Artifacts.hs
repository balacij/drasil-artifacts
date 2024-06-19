module Drasil.Artifacts where

import Prelude hiding (FilePath)

import Control.Monad.State
import Text.PrettyPrint

import qualified Data.Map.Strict as M

import Drasil.Fake.ChunkDB
import Drasil.Fake.System (System)

type FilePath = String

data Anchor = Anchor {
    file :: FilePath,
    fileQuery :: String,
    label :: Doc
} deriving Show

data Artifact = Artifact {
    files :: M.Map FilePath Doc,
    chunkAnchors :: M.Map UID Anchor -- Need to find the file and section that a chunk rendering belongs to? Check here!
} deriving Show

data ArtifactState = ArtifactState {
    artifact :: Artifact,
    chunkdb :: ChunkDB,
    system :: System
} deriving Show

type ArtifactBuilder = State ArtifactState

emptyArtifact :: Artifact
emptyArtifact = Artifact {
    files = M.empty,
    chunkAnchors = M.empty
}

basicArtifactState :: ChunkDB -> System -> ArtifactState
basicArtifactState = ArtifactState emptyArtifact

type ArtifactGenResult = (Doc, [(UID, String, Doc)])

addArtifact :: FilePath -> (ArtifactState -> ArtifactGenResult) -> ArtifactBuilder ()
addArtifact fp r = do
    artst <- get
    let art = artifact artst
        (doc, localAncs) = r artst
        ancs = M.fromList $ map (\(x,y,z) -> (x, Anchor fp y z)) localAncs
        art' = art { files = M.insert fp doc (files art), chunkAnchors = M.union (chunkAnchors art) ancs }
    put $ artst { artifact = art' }

build :: ArtifactState -> Artifact
build = artifact
