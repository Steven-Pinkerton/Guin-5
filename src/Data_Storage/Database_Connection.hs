{-# LANGUAGE OverloadedStrings #-}

module Data_Storage.Database_Connection where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Pool (Pool)
import Data.Yaml qualified as Yaml
import Database.Persist.Postgresql (ConnectionPool, PostgresConf (..), createPostgresqlPool)
import Database.Persist.Sql (SqlBackend, runSqlPool)

-- Configuration file path
configFilePath :: FilePath
configFilePath = "config/database.yaml"

-- Load database configuration
loadDatabaseConfig :: FilePath -> IO PostgresConf
loadDatabaseConfig filePath = do
    Yaml.decodeFileThrow filePath

-- Initialize database connection pool
initConnectionPool :: PostgresConf -> IO ConnectionPool
initConnectionPool config = runStderrLoggingT $ createPostgresqlPool (pgConnStr config) (pgPoolSize config)

-- A sample function to access the database
withDbConnection :: ReaderT SqlBackend IO a -> Pool SqlBackend -> IO a
withDbConnection = runSqlPool
