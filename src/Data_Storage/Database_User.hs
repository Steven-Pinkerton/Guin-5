{-# LANGUAGE OverloadedStrings #-}

module Data_Storage.Database_User where

import Control.Monad.Logger (logDebugN)
import Data.Pool (Pool)
import Data.Text (pack)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Data_Storage.Model (
    AuthQuestion,
    EntityField (
        AuthQuestionUserId,
        LibrarySizeBytes,
        LibraryUserId,
        MaskingHashCreatedAt,
        MaskingHashUserId,
        UserBackupEmail
    ),
    MaskingHash,
    Persona,
    ProfileCard,
    Unique (UniquePhoneNumber, UniqueUserHash),
    User,
    UserHash (UserHash, userHashUserId),
 )
import Data_Storage.Model qualified as Model
import Database.Persist.Postgresql (
    Entity,
    PersistEntity (Key),
    PersistQueryRead (selectFirst),
    PersistQueryWrite (deleteWhere, updateWhere),
    PersistStoreWrite (insert_),
    PersistUniqueRead (getBy),
    PersistUniqueWrite (insertUnique),
    SqlBackend,
    Update,
    runSqlPersistMPool,
    (==.),
    (>=.),
 )
import Database.Persist.Sql (Entity (entityVal), PersistField (toPersistValue), PersistStoreWrite (update), SelectOpt (Asc), Single (Single), fromSqlKey, rawSql, selectList, toSqlKey, (=.))

-- Insert a new user into the database and handle unique constraints
insertUser :: User -> Pool SqlBackend -> IO (Either Text (Key User))
insertUser user pool =
    liftIO $
        runSqlPersistMPool (insertUnique user) pool >>= \case
            Just userId -> return $ Right userId
            Nothing -> return $ Left "User already exists or other constraint violated."

-- Function to update a user's backup email
updateUserBackupEmail :: Model.UserId -> Maybe Text -> Pool SqlBackend -> IO ()
updateUserBackupEmail userId backupEmail pool =
    liftIO $
        runSqlPersistMPool (update userId [UserBackupEmail =. backupEmail]) pool

-- Retrieve a UserHash by its hash value
getUserHashByHash :: Text -> Pool SqlBackend -> IO (Maybe (Entity UserHash))
getUserHashByHash hash pool =
    liftIO $ runSqlPersistMPool (getBy $ UniqueUserHash hash) pool

-- Retrieve a User entity based on a hash value
getUserByUserHash :: Text -> Pool SqlBackend -> IO (Maybe (Entity User))
getUserByUserHash hash pool = do
    maybeUserHash <- liftIO $ getUserHashByHash hash pool
    case maybeUserHash of
        Just userHash -> liftIO $ getUserById (userHashUserId $ entityVal userHash) pool
        Nothing -> return Nothing

-- Insert a new profile card for a user
insertProfileCard :: ProfileCard -> Pool SqlBackend -> IO (Either Text (Key ProfileCard))
insertProfileCard profileCard pool =
    liftIO $
        runSqlPersistMPool (insertUnique profileCard) pool >>= \case
            Just profileCardId -> return $ Right profileCardId
            Nothing -> return $ Left "ProfileCard could not be created."

-- Insert a new persona for a user
insertPersona :: Persona -> Pool SqlBackend -> IO (Either Text (Key Persona))
insertPersona persona pool =
    liftIO $
        runSqlPersistMPool (insertUnique persona) pool >>= \case
            Just personaId -> return $ Right personaId
            Nothing -> return $ Left "Persona could not be created."

-- Function to save a new authentication question
saveAuthQuestion :: AuthQuestion -> Pool SqlBackend -> IO (Either Text ())
saveAuthQuestion authQuestion pool = liftIO $ runSqlPersistMPool action pool
  where
    action = do
        insert_ authQuestion
        return $ Right ()

-- Function to retrieve all authentication questions for a user
getAuthQuestionsByUserId :: Model.UserId -> Pool SqlBackend -> IO [Entity AuthQuestion]
getAuthQuestionsByUserId userId pool =
    liftIO $
        runSqlPersistMPool (selectList [AuthQuestionUserId ==. userId] []) pool

-- Function to update the user's document library size
updateLibrarySize :: Model.UserId -> Int -> Pool SqlBackend -> IO ()
updateLibrarySize userId newSize pool =
    liftIO $
        runSqlPersistMPool (updateWhere [LibraryUserId ==. userId] [LibrarySizeBytes =. newSize]) pool

-- Retrieve a user by ID
getUserById :: Model.UserId -> Pool SqlBackend -> IO (Maybe (Entity Model.User))
getUserById userId pool = liftIO $ runSqlPersistMPool (selectFirst [Model.UserId ==. userId] []) pool

-- Update a user's information
updateUser :: Model.UserId -> [Update Model.User] -> Pool SqlBackend -> IO ()
updateUser userId updates pool = liftIO $ runSqlPersistMPool (updateWhere [Model.UserId ==. userId] updates) pool

-- Delete a user by ID
deleteUser :: Model.UserId -> Pool SqlBackend -> IO ()
deleteUser userId pool = liftIO $ runSqlPersistMPool (deleteWhere [Model.UserId ==. userId]) pool

-- Retrieve a user by email
getUserByEmail :: Text -> Pool SqlBackend -> IO (Maybe (Entity User))
getUserByEmail email pool = liftIO $ runSqlPersistMPool action pool
  where
    action = getBy $ Model.UniqueEmail email

-- Retrieve a user by phone number
getUserByPhone :: Text -> Pool SqlBackend -> IO (Maybe (Entity User))
getUserByPhone phone pool = liftIO $ runSqlPersistMPool (getBy $ UniquePhoneNumber (Just phone)) pool

-- Function to save a new recovery code
saveRecoveryCode :: Model.UserId -> Text -> UTCTime -> Pool SqlBackend -> IO (Either Text ())
saveRecoveryCode userId code expiresAt pool = liftIO $ runSqlPersistMPool action pool
  where
    action = do
        insert_ $ Model.RecoveryCode userId code expiresAt
        return $ Right ()

-- Function to find and validate a recovery code
findRecoveryCode :: Model.UserId -> Text -> Pool SqlBackend -> IO (Maybe (Entity Model.RecoveryCode))
findRecoveryCode userId code pool = liftIO $ runSqlPersistMPool action pool
  where
    action = do
        currentTime <- liftIO getCurrentTime
        selectFirst [Model.RecoveryCodeUserId ==. userId, Model.RecoveryCodeCode ==. code, Model.RecoveryCodeExpiresAt >=. currentTime] []

-- Function to delete a recovery code
deleteRecoveryCode :: Model.UserId -> Pool SqlBackend -> IO ()
deleteRecoveryCode userId pool = liftIO $ runSqlPersistMPool action pool
  where
    action = deleteWhere [Model.RecoveryCodeUserId ==. userId]

-- Convert UserId to Text
userIdToText :: Model.UserId -> Text
userIdToText userId = show $ fromSqlKey userId

parseUserIdFromText :: Text -> Maybe (Key User)
parseUserIdFromText userIdText = toSqlKey <$> (readMaybe $ Prelude.toString userIdText :: Maybe Int64)

-- Function to insert a new masking hash for a user
insertMaskingHash :: Model.UserId -> Text -> Pool SqlBackend -> IO (Either Text (Key MaskingHash))
insertMaskingHash userId hash pool = do
    currentTime <- liftIO getCurrentTime
    -- Correct the SQL command to fit your DB's syntax. Assuming PostgreSQL:
    let sql = "INSERT INTO masking_hash (user_id, hash, created_at) VALUES (?, ?, ?) RETURNING id"
    let params = [toPersistValue userId, toPersistValue hash, toPersistValue currentTime]
    liftIO $
        flip runSqlPersistMPool pool $ do
            logDebugN $ "Executing SQL for insertMaskingHash with parameters: " <> show params
            result <- rawSql sql params
            return $ case result of
                [Single key] -> Right key -- Expecting a single result with the key
                _ -> Left "Failed to insert masking hash or retrieve ID."

-- Function to retrieve all masking hashes for a user
getMaskingHashesByUser :: Model.UserId -> Pool SqlBackend -> IO [Entity MaskingHash]
getMaskingHashesByUser userId pool =
    liftIO $
        runSqlPersistMPool (selectList [MaskingHashUserId ==. userId] [Asc MaskingHashCreatedAt]) pool

-- Function to insert a new UserHash
insertUserHash :: Model.UserId -> Text -> Pool SqlBackend -> IO (Either Text (Key UserHash))
insertUserHash userId hash pool = do
    currentTime <- liftIO getCurrentTime
    let newUserHash = UserHash userId hash currentTime
    result <- liftIO $ runSqlPersistMPool (insertUnique newUserHash) pool
    return $ case result of
        Just userHashId -> Right userHashId
        Nothing -> Left "UserHash could not be created."

generateUniqueMaskingHash :: IO Text
generateUniqueMaskingHash = do
    currentTime <- show . round <$> getPOSIXTime :: IO String
    randomUUID <- nextRandom
    let hash = currentTime ++ "-" ++ Data.UUID.toString randomUUID -- This should now work
    return $ pack hash