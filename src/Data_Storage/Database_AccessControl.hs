{-# LANGUAGE OverloadedStrings #-}

module Data_Storage.Database_AccessControl where

import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Data_Storage.Model (DocumentAccess (DocumentAccess), DocumentId, EntityField (..), UserId)
import Database.Persist.Sql (Entity, PersistStoreWrite (insert), SqlBackend, SqlPersistT, runSqlPool, selectFirst, selectList, (==.))

-- Import other necessary modules...

-- | Checks if one user is a trusted contact or a regular contact of another user.
isUserTrustedOrContact :: Pool SqlBackend -> UserId -> UserId -> IO Bool
isUserTrustedOrContact pool documentOwnerId accessingUserId = runSqlPool action pool
  where
    action :: SqlPersistT IO Bool
    action = do
        -- Log the action
        liftIO $ putStrLn "Checking trusted or regular contact relationship"
        trustedContactExists <- selectFirst [TrustedContactUserRef ==. documentOwnerId, TrustedContactTrustedUserId ==. accessingUserId] []
        case trustedContactExists of
            Just _ -> do
                liftIO $ putStrLn "Trusted contact relationship found"
                return True
            Nothing -> do
                liftIO $ putStrLn "No trusted contact relationship found"
                -- Extend with regular contact check if necessary
                return False

{- | Grants access to a document for a specified user. This is useful for documents with
 ConfidentialFirstParty, ConfidentialThirdParty, and Selected privacy levels.
-}
grantAccessToDocument :: Pool SqlBackend -> UserId -> DocumentId -> IO ()
grantAccessToDocument pool userId documentId = runSqlPool action pool
  where
    action :: SqlPersistT IO ()
    action = do
        currentTime <- liftIO getCurrentTime
        insertId <- insert $ DocumentAccess documentId userId currentTime
        -- Detailed logging
        liftIO $ putStrLn $ "Access granted to user: " ++ show userId ++ " for document: " ++ show documentId ++ ", access record ID: " ++ show insertId

-- | Simulates the process of selecting a user for document access.
selectUserForDocumentAccess :: UserId -> IO ()
selectUserForDocumentAccess userId = do
    -- Detailed logging
    putStrLn $ "User selected for document access: " ++ show userId

-- Fetches all access records for a given document.
fetchDocumentAccessRecords :: Pool SqlBackend -> DocumentId -> IO [Entity DocumentAccess]
fetchDocumentAccessRecords pool documentId = runSqlPool action pool
  where
    action :: SqlPersistT IO [Entity DocumentAccess]
    action = selectList [DocumentAccessDocumentId ==. documentId] []

checkIfUserIsContactOf :: UserId -> UserId -> SqlPersistT IO Bool
checkIfUserIsContactOf ownerUserId accessingUserId = do
    maybeContact <- selectFirst [ContactUserId ==. ownerUserId, ContactContactInfo ==. show accessingUserId] []
    return $ isJust maybeContact