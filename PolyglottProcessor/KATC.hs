{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import POParser
import Data.Default
import Data.Maybe
import Data.Typeable
import Control.Applicative
import Control.Exception (Exception, throw)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.Base.RegexLike (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text
import Data.Either.Combinators

data RuleException = RuleNotCompilable String
    deriving (Typeable)
instance Show RuleException where
    show (RuleNotCompilable msg) = "Can't compile rule: " ++ msg
instance Exception RuleException

data Severity = SeverityError | SeverityWarning | SeverityStandard | SeverityInfo | SeverityNotice
    deriving (Show, Eq, Ord)

data Rule = SimpleRegexRule Text Regex Severity

instance Show Rule where
    show (SimpleRegexRule name _ sev) = "(SimpleRegexRule " ++ T.unpack name ++ " | " ++ show sev

data RuleHit = RuleHit SimplePORecord Text
    deriving (Show)

instance Default CompOption where
    def = defaultCompOpt

-- caseInsensitive :: CompOption
-- caseInsensitive = defaultCompOpt {caseSensitive = False}

-- ^ Compile with exception
compileWithExc :: CompOption -> Text -> Regex
compileWithExc co rgx =
    case compile co defaultExecOpt rgx of
        Left msg -> throw $ RuleNotCompilable msg
        Right reg -> reg

simpleRegexRule :: Text -> Text -> Severity -> CompOption -> Rule
simpleRegexRule name rgx sev co =
    SimpleRegexRule name (compileWithExc co rgx) sev

rules :: [Rule]
rules = []

rule1 = simpleRegexRule "Occurrence of untranslated 'intercept" "\\b[Ii]ntercepts?\\b" SeverityStandard def

applyRule :: Rule -> SimplePORecord -> Maybe RuleHit
applyRule (SimpleRegexRule txt regex sev) por =
    case regexec regex (simplePOMsgstr por) of
        Left _ -> Nothing -- Some internal regex error
        Right Nothing -> Nothing -- No hit found
        Right (Just (_, hit, _, _)) -> Just $ RuleHit por hit

main :: IO ()
main = do
    poRecords <- parsePOFile "../cache/de/2_high_priority_content/learn.math.cc-eighth-grade-math.exercises.pot"
    print $ poRecords
    let simplePOR = mapMaybe poToSimple poRecords :: [SimplePORecord]
    print $ mapMaybe (applyRule rule1) simplePOR