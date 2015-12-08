{-# LANGUAGE OverloadedStrings #-}
import POParser
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import System.FilePath.Posix
import Data.Either.Combinators
import Control.Concurrent.Async
import System.Directory.Extra (listFilesRecursive)
import qualified Data.Map.Strict as M

-- A map from language to translation, e.g. "de" -> "Polynom", "en" -> "Polynome"
type LanguageMap = [(Text, Text)]
-- A translation map which associates a key with a list of translations
type TranslationMap = M.Map Text LanguageMap

processPOFile :: FilePath -> IO [(Text, Text)]
processPOFile fp = processPOData <$> B.readFile fp

-- Process POT file content, search for titles and return [(msgid, msgstr)]
processPOData :: ByteString -> [(Text, Text)]
processPOData bs =
    let allowedTypes = ["Title of topic"] -- "Description of topic"
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

-- Process a directory of PO files
processPODirectories :: FilePath -> [Text] -> IO [TranslationMap]
processPODirectories dir langs = forM langs $ \lang -> do
        let curdir = dir </> T.unpack lang
        results <- processPODirectory curdir
        return $ poDirResultToTranslationMap lang results

poDirResultToTranslationMap :: Text -> [(Text, Text)] -> TranslationMap
poDirResultToTranslationMap lang results =
    let f (k, v) = (k, [(lang, v)])
    in M.fromList $ map f results

unionTranslationMap :: TranslationMap -> TranslationMap -> TranslationMap
unionTranslationMap = M.unionWith (++)

main :: IO ()
main = do
    results <- processPODirectories "../cache" ["de", "fr"]
    print $ foldr1 unionTranslationMap results
    