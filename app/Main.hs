{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (except)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson (eitherDecode)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP (hAuthorization)
import Network.URI (parseURI)
import Pipes.Safe (MonadMask)
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import Systemd.Journal.Upload
  ( JournalUploadConfig (destinationUri, journalUploadRequestHeaders, journalUploadFilter),
    JournalUploadError,
    JournalFilter(..),
    EntryFilter(..),
    defaultJournalUploadConfig,
    runWithConfig,
  )

data PapertrailUploadError
  = DestinationURIInvalid
  | InvalidConfig String
  | TokenUnavailable
  | WrappedJournalUploadError JournalUploadError
  deriving (Show)

data PapertrailUploadConfig = PapertrailUploadConfig {
  filters :: [[(Text, Text)]]
} deriving (Show, Generic, FromJSON)

loadPapertrailConfig :: MonadIO m => FilePath -> ExceptT PapertrailUploadError m PapertrailUploadConfig 
loadPapertrailConfig path = ExceptT $ liftIO $ first InvalidConfig <$> Aeson.eitherDecode <$> B.readFile path

makeJournalUploadConfig :: ByteString -> PapertrailUploadConfig -> Maybe JournalUploadConfig
makeJournalUploadConfig token (PapertrailUploadConfig { filters }) =
  let maybeUri = parseURI "https://logs.collector.solarwinds.com/v1/log"
   in fmap
        ( \uri ->
            defaultJournalUploadConfig
              { destinationUri = uri,
                journalUploadRequestHeaders = [(HTTP.hAuthorization, "Basic " <> token)],
                journalUploadFilter = Just $ makeJournalUploadFilter filters
              }
        )
        maybeUri
  where
    makeJournalUploadFilter = JournalFilter . fmap EntryFilter  

runPapertrail :: (MonadIO m, MonadMask m) => ExceptT PapertrailUploadError m ()
runPapertrail = do
  maybeToken <- liftIO $ lookupEnv "PAPERTRAIL_TOKEN"
  token <- maybe (except $ Left TokenUnavailable) (pure . UTF8.fromString) maybeToken
  papertrailConfigPath <- liftIO $ getUserConfigFile "journal-upload-papertrail" "config.json"
  papertrailConfig <- loadPapertrailConfig papertrailConfigPath
  config <- maybe (except $ Left DestinationURIInvalid) pure $ makeJournalUploadConfig token papertrailConfig
  ExceptT $ fmap (first WrappedJournalUploadError) $ runExceptT $ runWithConfig config

main :: IO ()
main = runExceptT runPapertrail >>= either (error . show) pure
