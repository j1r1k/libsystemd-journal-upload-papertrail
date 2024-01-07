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
import qualified Network.HTTP.Types as HTTP (hAuthorization)
import Network.URI (parseURI)
import Pipes.Safe (MonadMask)
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import Systemd.Journal.Upload
  ( JournalUploadConfig (destinationUri, journalUploadProducerId, journalUploadRequestHeaders, journalUploadFilters),
    JournalUploadError,
    defaultJournalUploadConfig,
    runWithConfig
  )
import Systemd.Journal.Filter (EntryFilter(..))

data PapertrailUploadError
  = DestinationURIInvalid
  | InvalidConfig String
  | TokenUnavailable
  | WrappedJournalUploadError JournalUploadError
  deriving (Show)

data ExternalConfig = ExternalConfig {
  filters :: [EntryFilter]
} deriving (Show, Generic, FromJSON)

loadExternalConfig :: MonadIO m => ExceptT PapertrailUploadError m ExternalConfig 
loadExternalConfig = do
  path <- liftIO $ getUserConfigFile "journal-upload" "config.json"
  ExceptT $ liftIO $ first InvalidConfig <$> Aeson.eitherDecode <$> B.readFile path



makeJournalUploadConfig :: ByteString -> ExternalConfig -> Maybe JournalUploadConfig
makeJournalUploadConfig token (ExternalConfig { filters }) =
  let maybeUri = parseURI "https://logs.collector.solarwinds.com/v1/log"
   in fmap
        ( \uri ->
            defaultJournalUploadConfig
              { journalUploadProducerId = "papertrail",
                destinationUri = uri,
                journalUploadRequestHeaders = [(HTTP.hAuthorization, "Basic " <> token)],
                journalUploadFilters = filters
              }
        )
        maybeUri

loadConfig :: MonadIO m => ExceptT PapertrailUploadError m JournalUploadConfig
loadConfig = do
  maybeToken <- liftIO $ lookupEnv "PAPERTRAIL_TOKEN"
  token <- maybe (except $ Left TokenUnavailable) (pure . UTF8.fromString) maybeToken
  externalConfig <- loadExternalConfig
  maybe (except $ Left DestinationURIInvalid) pure $ makeJournalUploadConfig token externalConfig

runPapertrail :: (MonadIO m, MonadMask m) => ExceptT PapertrailUploadError m ()
runPapertrail = do
  config <- loadConfig
  ExceptT $ fmap (first WrappedJournalUploadError) $ runExceptT $ runWithConfig config

main :: IO ()
main = runExceptT runPapertrail >>= either (error . show) pure
