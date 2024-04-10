module TestUtils where

import Data.Pool (Pool)
import Data.Text (pack)
import Data.Time (UTCTime (utctDayTime), getCurrentTime)
import Data_Storage.Model (Contact (Contact), ContactId, ContentType, Document (Document), DocumentId, EntityField (DocumentAccessDocumentId, DocumentId), MaskingHash, PrivacyLevel, User (User), UserId, documentContent, documentContentType, documentCreatedAt, documentPrivacyLevel, documentUserRef, userBackupEmail, userCountry, userCreated_at, userDob, userEmail, userName, userPassword, userPhoneNumber, userPhotoUrl, userUpdated_at)
import Database.Persist.Postgresql (
    Filter,
    PersistQueryWrite (deleteWhere),
    PersistStoreWrite (insert),
    SqlBackend,
    runSqlPersistMPool,
    runSqlPool,
    (<-.),
    (==.),
 )
import System.Random (randomIO)

-- Generates a unique phone number for testing purposes.
-- It appends a unique suffix based on the current time to a base number.
generateTestPhoneNumber :: IO Text
generateTestPhoneNumber = do
    currentTime <- getCurrentTime
    let uniqueSuffix = show . floor . utctDayTime $ currentTime
    return $ "+123456789" <> pack uniqueSuffix

-- Clear dependent data from 'masking_hash' before clearing the 'user' data
clearDependentData :: Pool SqlBackend -> IO ()
clearDependentData pool = liftIO $ runSqlPersistMPool (deleteWhere ([] :: [Filter MaskingHash])) pool

-- Clean up function, adjust as needed.
cleanUp :: (Pool SqlBackend, DocumentId) -> IO ()
cleanUp _ = putStrLn "Clean up after tests."

createTestUser :: Pool SqlBackend -> IO UserId
createTestUser pool = do
    -- Generate a unique identifier for the user (e.g., using the current time or a random number).
    uid <- show <$> (randomIO :: IO Int)
    let email = "test" <> pack uid <> "@example.com"

    currentTime <- getCurrentTime
    runSqlPool
        ( insert
            User
                { userEmail = email
                , userBackupEmail = Nothing
                , userPhoneNumber = Just $ "+11234567890" <> pack uid -- Optionally make phone numbers unique as well
                , userPassword = "hashed_password_example"
                , userName = Just "Test User"
                , userDob = Nothing
                , userCountry = Just "Test Country"
                , userPhotoUrl = Just "https://example.com/photo.jpg"
                , userCreated_at = currentTime
                , userUpdated_at = currentTime
                }
        )
        pool

createTestDocument :: UserId -> Text -> PrivacyLevel -> ContentType -> Pool SqlBackend -> IO DocumentId
createTestDocument userId content privacyLevel contentType pool = do
    currentTime <- getCurrentTime
    runSqlPool
        ( insert
            Document
                { documentUserRef = userId
                , documentContentType = contentType
                , documentContent = content
                , documentPrivacyLevel = privacyLevel
                , documentCreatedAt = currentTime
                }
        )
        pool

assertFailure :: Text -> IO ()
assertFailure = error

deleteDocumentAccessForDoc :: DocumentId -> Pool SqlBackend -> IO ()
deleteDocumentAccessForDoc docId pool = do
    putStrLn $ "Deleting DocumentAccess records for document: " ++ show docId
    runSqlPool (deleteWhere [DocumentAccessDocumentId ==. docId]) pool
    putStrLn "Deletion of DocumentAccess records completed."

deleteDocuments :: [DocumentId] -> Pool SqlBackend -> IO ()
deleteDocuments docIds pool = do
    putStrLn "Deleting documents..."
    runSqlPool (deleteWhere [DocumentId <-. docIds]) pool
    putStrLn "Deletion of documents completed."

addTestContact :: Pool SqlBackend -> UserId -> Text -> IO ContactId
addTestContact pool userId contactInfo = do
    let newContact = Contact userId contactInfo
    contactId <- runSqlPersistMPool (insert newContact) pool
    putStrLn $ "Inserted Contact for User: " <> show userId <> ", Contact ID: " <> show contactId
    return contactId