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
import Data.ByteString.Base64 as B64 (encode)
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
  uri :: String,
  filters :: [EntryFilter]
} deriving (Show, Generic, FromJSON)

loadExternalConfig :: MonadIO m => ExceptT PapertrailUploadError m ExternalConfig
loadExternalConfig = do
  path <- liftIO $ getUserConfigFile "journal-upload" "config.json"
  ExceptT $ liftIO $ first InvalidConfig . Aeson.eitherDecode <$> B.readFile path

makeJournalUploadConfig :: ByteString -> ExternalConfig -> Maybe JournalUploadConfig
makeJournalUploadConfig token (ExternalConfig { uri, filters }) =
  let maybeUri = parseURI uri
   in fmap
        ( \parsedUri ->
            defaultJournalUploadConfig
              { journalUploadProducerId = "papertrail",
                destinationUri = parsedUri,
                journalUploadRequestHeaders = [
                  (HTTP.hAuthorization, "Basic " <> (B64.encode (":" <> token)))
                  ],
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
