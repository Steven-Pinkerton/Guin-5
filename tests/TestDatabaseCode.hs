module TestDatabaseCode where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.List qualified
import Data.Map (fromList, isSubmapOf)
import Data.Pool (Pool)
import Data.Time (UTCTime)
import Data_Storage.Model (Document, EntityField (UserPhoneNumber), Post, User, migrateAll, userPhoneNumber)
import Database.Persist qualified as Sql
import Database.Persist.Postgresql (ConnectionString, Filter, Single, createPostgresqlPool, rawSql, runSqlPool, (==.))
import Database.Persist.Sql (Entity (entityVal), PersistQueryRead (count), PersistQueryWrite (deleteWhere), PersistValue (PersistText), SelectOpt (Asc), Single (Single), SqlBackend, SqlPersistT, runMigration, selectList)

expectedDefaults :: Map String String
expectedDefaults =
    Data.Map.fromList
        [ ("created_at", "CURRENT_TIMESTAMP")
        , ("updated_at", "CURRENT_TIMESTAMP")
        ]

checkSchema :: SqlPersistT IO Bool
checkSchema = do
    let query = "SELECT column_name, column_default FROM information_schema.columns WHERE table_name = ?;"
    rows <- rawSql query [PersistText "user"]
    liftIO $ print rows -- Debug: Print query results

    -- Adjust to handle Maybe types for default values
    let actualDefaults = Data.Map.fromList [(column, fromMaybe "" def) | (Single column, Single def) <- rows]

    return $ expectedDefaults `Data.Map.isSubmapOf` actualDefaults

setupDatabaseWithCheck :: Pool SqlBackend -> IO ()
setupDatabaseWithCheck =
    runSqlPool
        ( do
            schemaOk <- checkSchema
            unless schemaOk $ do
                liftIO $ putStrLn "Schema does not match expectations. Adjusting..."
                -- Place schema adjustment logic here, if any.
                -- This could involve running `customMigration` or specific `ALTER TABLE` commands.
        )

customMigration :: SqlPersistT IO ()
customMigration = do
    runMigration migrateAll

-- Commenting out these lines to prevent conflict with existing schema
-- rawExecute "ALTER TABLE document ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP" []
-- rawExecute "ALTER TABLE user ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP" []
-- rawExecute "ALTER TABLE user ALTER COLUMN updated_at SET DEFAULT CURRENT_TIMESTAMP" []

-- Function to create a pool of database connections for testing
createTestDatabasePool :: IO (Pool SqlBackend)
createTestDatabasePool = runStdoutLoggingT $ do
    let connectionString = "host=localhost port=5432 dbname=testdb user=steven password=IronGuard01!"
    Database.Persist.Postgresql.createPostgresqlPool connectionString 10

-- Function to create a pool of database connections for testing; for use in PrivacySpecTest
createTestDatabasePool2 :: ConnectionString -> IO (Pool SqlBackend)
createTestDatabasePool2 connectionString = runStdoutLoggingT $ do
    Database.Persist.Postgresql.createPostgresqlPool connectionString 10

-- Function to check database connection by querying the current time
checkDatabaseConnection :: Pool SqlBackend -> IO ()
checkDatabaseConnection =
    Database.Persist.Postgresql.runSqlPool
        ( do
            currentTime <- Database.Persist.Postgresql.rawSql "SELECT NOW()" []
            liftIO $ putStrLn $ "Database connection check, current time: " ++ show (currentTime :: [Database.Persist.Postgresql.Single UTCTime])
        )

-- Function to log the count of users in the 'user' table
logUserCount :: Pool SqlBackend -> IO ()
logUserCount =
    Database.Persist.Postgresql.runSqlPool
        ( do
            userCount <- Sql.count ([] :: [Database.Persist.Postgresql.Filter User]) -- Use the qualified name here
            liftIO $ putStrLn $ "Number of users: " ++ show userCount
        )

logAllPhoneNumbers :: Pool SqlBackend -> IO ()
logAllPhoneNumbers =
    runSqlPool
        ( do
            phones <- selectList [] [Asc UserPhoneNumber]
            liftIO . putStrLn $ "Existing phone numbers in the database: " ++ show (Data.List.map (userPhoneNumber . entityVal) phones)
        )

logBeforeTest :: Pool SqlBackend -> IO ()
logBeforeTest pool = do
    logUserCount pool
    logAllPhoneNumbers pool

setupDatabase :: Pool SqlBackend -> IO ()
setupDatabase pool = runSqlPool customMigration pool

-- Function to clear the 'user' table, now properly discarding the Int64 result
clearUserTable :: SqlPersistT IO ()
clearUserTable = deleteWhere ([] :: [Filter User])

checkPhoneNumberUniqueness :: Pool SqlBackend -> Text -> IO Bool
checkPhoneNumberUniqueness pool phoneNumber =
    Database.Persist.Postgresql.runSqlPool
        ( do
            userCount <- Database.Persist.Sql.count [UserPhoneNumber Database.Persist.Postgresql.==. Just phoneNumber]
            return $ userCount == 0
        )
        pool

-- Function to clear test data from the database
clearDatabase :: Pool SqlBackend -> IO ()
clearDatabase =
    runSqlPool
        ( do
            deleteWhere ([] :: [Filter Document])
            deleteWhere ([] :: [Filter Post]) -- Ensure all posts are deleted first
            deleteWhere ([] :: [Filter User]) -- Now safe to delete users
            -- Add more `deleteWhere` calls for other entities as needed
        )
