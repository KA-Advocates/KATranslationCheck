{-# LANGUAGE OverloadedStrings #-}
import POParser
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Data.Aeson
import Control.DeepSeq (force)
import System.FilePath.Posix
import Data.Either.Combinators
import Control.Concurrent.Async
import System.Directory.Extra (listFilesRecursive)
import qualified Data.Map.Strict as M

-- A map from language to translation, e.g. "de" -> "Polynom", "en" -> "Polynome"
type LanguageMap = M.Map Text Text
-- A translation map which associates a key with a list of translations
type TranslationMap = M.Map Text LanguageMap
-- Index for a translations map, maps translation to the english version (which you can lookup in the main map)
type TranslationMapIndex = M.Map Text Text

processPOFile :: FilePath -> IO [(Text, Text)]
processPOFile fp = processPOData <$> LTIO.readFile fp

-- Process POT file content, search for titles and return [(msgid, msgstr)]
processPOData :: LT.Text -> [(Text, Text)]
processPOData bs =
    let allowedTypes = ["Title of topic", "Title of video", "Description of topic"]
        test a = any (\t -> T.isInfixOf t a) allowedTypes
        poResult = fromRight [] $ parsePOFile bs
        poEntries = mapMaybe poToSimple $ poResult
        filteredPO = filter (test . simplePOComment) $ poEntries
        filteredPO2 = filter (not . T.null . simplePOMsgstr) $ filteredPO
        toTuple r = (simplePOMsgid r, simplePOMsgstr r)
    in map toTuple filteredPO2

-- Process a directory of PO files
processPODirectory :: FilePath -> IO [(Text, Text)]
processPODirectory dir = do
    files <- listFilesRecursive dir
    concat <$> mapConcurrently processPOFile files

forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently = flip mapConcurrently

-- Process a directory of PO files
processPODirectories :: FilePath -> [Text] -> IO [TranslationMap]
processPODirectories dir langs = forConcurrently langs $ \lang -> do
        let curdir = dir </> T.unpack lang
        results <- processPODirectory curdir
        return $ force $ poDirResultToTranslationMap lang results

poDirResultToTranslationMap :: Text -> [(Text, Text)] -> TranslationMap
poDirResultToTranslationMap lang results =
    let f (k, v) = (k, M.fromList [(lang, v)])
    in M.fromList $ map f results

unionTranslationMap :: TranslationMap -> TranslationMap -> TranslationMap
unionTranslationMap = M.unionWith M.union

buildInvertedIndex :: TranslationMap -> TranslationMapIndex
buildInvertedIndex tm =
    let f :: (Text, M.Map Text Text) -> [(Text, Text)]
        f (k, vals) = (k, k) : (map (\(_, v) -> (v, k)) $ M.assocs vals) -- Ignore language
    in M.fromList $ concatMap f $ M.assocs tm 

main :: IO ()
main = do
    results <- processPODirectories "../cache" ["de", "fr", "nl", "pt-BR", "ru", "hr", "ro", "pl", "sv-SE", "nn-NO", "es"]
    let tm = foldr1 unionTranslationMap results
    let index = buildInvertedIndex tm
    LB.writeFile "TranslationMap.json" $ encode tm
    LB.writeFile "TranslationIndex.json" $ encode index
    