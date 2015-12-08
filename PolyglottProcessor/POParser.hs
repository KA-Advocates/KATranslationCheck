{-# LANGUAGE OverloadedStrings #-}
module POParser where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text.Lazy hiding (skipWhile)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import Data.Word
import Data.Monoid
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Either
import qualified Data.Text as T

data PORecord = PORecord {
        poComment :: Text,
        poMsgid :: Text,
        poMsgidPlural :: Maybe Text,
        poMsgstr :: Maybe Text,
        poMsgstrPlural :: [Text]
    } deriving (Show, Eq)

-- ^ A simpler form of PO records that does not support
data SimplePORecord = SimplePORecord {
    simplePOComment :: Text,
    simplePOMsgid :: Text,
    simplePOMsgstr :: Text
} deriving (Show, Eq)

poToSimple :: PORecord -> Maybe SimplePORecord
poToSimple (PORecord comm msgid _ (Just msgstr) _) = Just $ SimplePORecord comm msgid msgstr
poToSimple _ = Nothing

-- Make a parser optional, return Nothing if there is no match
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

takeTillEOL :: Parser Text
takeTillEOL = takeWhile (not . isEndOfLine)

parseKeyedLine :: Text -> Parser Text
parseKeyedLine key = do
  void (string key) <?> "Line key"
  skipSpace
  char '"' <?> "Opening Qutotation mark"
  val <- takeTillEOL
  endOfLine <?> "EOL"
  return $ T.dropEnd 1 val

-- Parse lines of the form msgstr[n] "..."
parseMsgstrPlural :: Parser (Int, Text)
parseMsgstrPlural = do
  void (string "msgstr") <?> "Line key"
  n <- char '[' *> decimal <* char ']'
  skipSpace
  char '"' <?> "Opening Qutotation mark"
  val <- takeTillEOL
  endOfLine <?> "EOL"
  return (n, T.dropEnd 1 val)

-- Parses a POT line 
lineWithExtraLines :: Parser Text -> Parser Text
lineWithExtraLines p = do
    txt <- p <?> "Main line"
    extraTxt <- many escapedTextLine <?> "Extra lines"
    return $ T.intercalate "\n" $ txt : extraTxt

commentLines = (T.intercalate "\n" <$> many1 commentLine) <?> "comments"
msgidLines = (lineWithExtraLines $ parseKeyedLine "msgid") <?> "msgid"
msgidPluralLines = (lineWithExtraLines $ parseKeyedLine "msgid_plural") <?> "msgid_plural"
msgstrLines = (lineWithExtraLines $ parseKeyedLine "msgstr") <?> "msgstr"
msgstrPluralLines =
    -- Ignore index: assume order is correct
    let singlePlural = lineWithExtraLines (snd <$> parseMsgstrPlural) <?> "single msgstr plural"
    in many singlePlural

escapedTextLine :: Parser Text
escapedTextLine = char '"' *> takeTillEOL <* endOfLine

nameP :: String -> Parser a -> Parser a
nameP str p = p <?> str

commentLine :: Parser Text
commentLine = nameP "comment line" $ do
    char '#' <?> "Line start hash"
    -- Skip space but not newline
    void $ many (char ' ')
    txt <- takeTillEOL
    endOfLine <?> "EOF"
    return txt

emptyLine :: Parser ()
emptyLine = skipSpace <* endOfLine

poRecord :: Parser PORecord
poRecord = do
    comment <- commentLines
    msgid <- msgidLines
    msgidPlural <- maybeOption msgidPluralLines 
    msgstr <- maybeOption msgstrLines
    msgstrPlural <- msgstrPluralLines
    endOfLine
    return $ PORecord comment msgid msgidPlural msgstr msgstrPlural

poFile :: Parser [PORecord]
poFile = many1 poRecord <?> "PO results"

parsePOFile :: LT.Text -> Either Text [PORecord]
parsePOFile txt =
  case parse poFile txt of
    Fail _ [] err   -> Left $ T.pack err
    Fail _ ctxs err -> Left (T.intercalate " > " (map T.pack ctxs) <> ": " <> T.pack err)
    Done _ a        -> Right a
