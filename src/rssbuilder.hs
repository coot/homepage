{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Main where

import           System.Environment
import           System.Exit

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import           GHC.Generics (Generic (..))


data Entry = Entry {
    eTitle     :: Text,
    eUrl       :: Text,
    eDate      :: Text,
    eBuildDate :: Text
  }
  deriving Generic

instance FromJSON Entry where
    parseJSON = withObject "Entry" $ \v ->
      Entry <$> v .: "title"
            <*> v .: "url"
            <*> v .: "date"
            <*> pure mempty

instance ToJSON Entry where
    toEncoding Entry { eTitle
                     , eUrl
                     , eDate
                     , eBuildDate
                     } =
      pairs $ "title"         .= eTitle
           <> "url"           .= eUrl
           <> "date"          .= eDate
           <> "lastBuildDate" .= eBuildDate

data Entries a = Entries {
    entries :: [a],
    pubDate :: Text
  }
  deriving (Generic)

instance FromJSON a => FromJSON (Entries a) where
    parseJSON = withObject "Entries" $ \v ->
      Entries <$> v .: "entries"
              <*> pure mempty

instance ToJSON a => ToJSON (Entries a) where
    toEncoding Entries { entries, pubDate } =
      pairs $ "entries" .= entries
           <> "pubDate" .= pubDate


main :: IO ()
main = do
    args <- getArgs
    (inputFile, outputFile)  <-
      case args of
        [ ]         -> putStrLn "rssbuilder input output"
                    >> exitFailure
        [_]         -> putStrLn "rssbuilder intput output"
                    >> exitFailure
        (a : b : _) -> return (a, b)
    (mbFeed :: Either String (Entries Entry))
      <- eitherDecodeFileStrict' inputFile
    feed <- case mbFeed of
      Left err   -> putStrLn ("json parse error: " ++ err)
                 >> exitFailure
      Right feed -> return feed
    t <- Time.getCurrentTime
    let eBuildDate = Text.pack $ Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat t
        feed'      = Entries { entries = updateEntry eBuildDate <$> entries feed
                             , pubDate = eBuildDate
                             }
    case outputFile of
      "-" -> BS.putStr $ encode feed'
      _   -> encodeFile outputFile feed'
  where
    updateEntry :: Text -> Entry -> Entry
    updateEntry eBuildDate e = e { eBuildDate }

