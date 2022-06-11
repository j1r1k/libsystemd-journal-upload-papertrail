{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (except)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import qualified Network.HTTP.Types as HTTP (hAuthorization)
import Network.URI (parseURI)
import Pipes.Safe (MonadMask)
import System.Environment (lookupEnv)
import Systemd.Journal.Upload
  ( JournalUploadConfig (destinationUri, journalUploadRequestHeaders),
    JournalUploadError,
    defaultJournalUploadConfig,
    runWithConfig,
  )

data PapertrailUploadError
  = DestinationURIInvalid
  | TokenUnavailable
  | WrappedJournalUploadError JournalUploadError
  deriving (Show)

papertrailUploadConfig :: ByteString -> Maybe JournalUploadConfig
papertrailUploadConfig token =
  let maybeUri = parseURI "https://logs.collector.solarwinds.com/v1/log"
   in fmap
        ( \uri ->
            defaultJournalUploadConfig
              { destinationUri = uri,
                journalUploadRequestHeaders = [(HTTP.hAuthorization, "Basic " <> token)]
              }
        )
        maybeUri

runPapertrail :: (MonadIO m, MonadMask m) => ExceptT PapertrailUploadError m ()
runPapertrail = do
  maybeToken <- liftIO $ lookupEnv "PAPERTRAIL_TOKEN"
  token <- maybe (except $ Left TokenUnavailable) (pure . UTF8.fromString) maybeToken
  config <- maybe (except $ Left DestinationURIInvalid) pure $ papertrailUploadConfig token
  ExceptT $ fmap (first WrappedJournalUploadError) $ runExceptT $ runWithConfig config

main :: IO ()
main = runExceptT runPapertrail >>= either (error . show) pure
