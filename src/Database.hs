{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database where

import           Control.Monad.Logger        (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Pool
import           Data.Text(Text(..), pack)
import           Data.ByteString (ByteString(..))
import           Data.Time (UTCTime)
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Database.Persist.Quasi (lowerCaseSettings)
import           Yesod

-- https://stackoverflow.com/questions/9105903/why-arent-persistent-types-instances-of-tojson-fromjson-in-yesod


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

newtype Config = Config { getPool :: Pool SqlBackend }

runDb :: (MonadUnliftIO m, MonadReader Config m) => ReaderT SqlBackend m b -> m b
runDb query = do
   pool <- asks getPool
   runSqlPool query pool

makeSqlitePool :: IO (Pool SqlBackend)
makeSqlitePool = do
   p <- runStderrLoggingT $ createSqlitePool ":memory:" 10
   runSqlPool (runMigration migrateAll) p
   return p

makeSqlitePoolFromFile :: String -> IO (Pool SqlBackend)
makeSqlitePoolFromFile fn = do
  p <- runStderrLoggingT $ createSqlitePool (pack fn) 10
  runSqlPool (runMigration migrateAll) p
  return p

makePostgresPool :: Bool -> IO (Pool SqlBackend)
makePostgresPool doMigrate = do
  p <- runStderrLoggingT $ createPostgresqlPool "postgres://mydbuser:mydbpass@localhost:5432/mydb" 10
  when doMigrate $ runSqlPool (runMigration migrateAll) p
  return p
