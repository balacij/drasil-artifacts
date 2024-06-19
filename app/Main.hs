module Main (main) where

import qualified Data.Map.Strict as M

import Drasil.Artifacts
import Drasil.Fake.ChunkDB
import Drasil.Fake.Theories (Theory)

import qualified Drasil.Fake.Theories as Th
import qualified Drasil.Fake.System as Sys
import Text.PrettyPrint
import Prelude hiding ((<>))
import Control.Monad.State (execState)
import Data.Bifunctor (first)

th1, th2, th3 :: Th.Theory
th1 = Th.Theory { Th._uid = "th1", Th.title = "th1", Th.things = ["thing 1 1"] }
th2 = Th.Theory { Th._uid = "th2", Th.title = "th2", Th.things = ["thing 2 1", "thing 2 2"] }
th3 = Th.Theory { Th._uid = "th3", Th.title = "th3", Th.things = ["thing 3 1", "thing 3 2"] }

-- >>> show th1
-- "Theory {uid = \"th1\", title = \"th1\", things = [\"thing 1 1\"]}"

cdb1 :: ChunkDB
cdb1 = M.fromList $ map (first uid . (\x -> (x, Th.thToChunk x))) [th1, th2, th3]

-- >>> show cdb
-- "fromList [(\"th1\",fromList [(\"things\",\"thing 1 1\"),(\"title\",\"th1\"),(\"uid\",\"th1\")]),(\"th2\",fromList [(\"things\",\"thing 2 1/thing 2 2\"),(\"title\",\"th2\"),(\"uid\",\"th2\")]),(\"th3\",fromList [(\"things\",\"thing 3 1/thing 3 2\"),(\"title\",\"th3\"),(\"uid\",\"th3\")])]"

sys1 :: Sys.System
sys1 = Sys.System {
        Sys._uid = "sys1",
        Sys.author = "Jason Balaci",
        Sys.purpose = "To show off an artifacts writer.",
        Sys.title = "Artwriter",
        Sys.theories = [th1, th2, th3]
    }

-- >>> show sys1

-- sysToMdBook sys cdb = do
--     -- Create the README.md
--     writeArtifact "README.md" sys cdb readmeWriter -- places no anchors
--     writeArtifact "src/Introduction.md" sys cdb introWriter -- defines the system, places an anchor to the "System" definition in the state
--     writeArtifact "src/Theories.md" sys cdb theoriesWriter -- renders the theories, places 1 anchor per rendered "Theory" in the state

main :: IO ()
main = ex1

ex1 :: IO ()
ex1 = do
    let mdbookArt = genMdBook cdb1 sys1
    putStrLn "Artifacts:"
    mapM_ print (M.toList $ files mdbookArt)
    putStrLn "\nAnchors:"
    mapM_ print (M.toList $ chunkAnchors mdbookArt)

genMdBook :: ChunkDB -> Sys.System -> Artifact
genMdBook cdb sys =
    let start = basicArtifactState cdb sys
        recipe = do
            addArtifact "README.md" genReadMe
            addArtifact "src/Introduction.md" genIntroduction
            addArtifact "src/Theories.md" genTheories
    in build $ execState recipe start

genReadMe :: ArtifactState -> ArtifactGenResult
genReadMe as = (vcat [
        text "# " <> text (Sys.title $ system as),
        text "Author: " <> text (Sys.author $ system as)
    ], [])

genIntroduction :: ArtifactState -> ArtifactGenResult
genIntroduction as = (vcat [
        text "# " <> text (Sys.title $ system as),
        text "Author: " <> text (Sys.author $ system as),
        text "## Introduction",
        text $ Sys.purpose $ system as
    ], [(uid $ system as, "fake-query-to-h1", text (Sys.title $ system as))])

genTheories :: ArtifactState -> ArtifactGenResult
genTheories as = (vcat $ text "# Theories" : map renderTheory (Sys.theories $ system as), map (\t -> (uid t, "fake-query-to-" ++ Th.title t, text $ Th.title t)) $ Sys.theories $ system as)

renderTheory :: Theory -> Doc
renderTheory th = (text "# " <> text (Th.title th)) $$ text (unwords $ Th.things th)
