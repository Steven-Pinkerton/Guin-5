{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use alternative" #-}

module Data_Storage.DocumentManagement where

import Conduit (ConduitT, ResourceT, await, runConduitRes, (.|))
import Crypto.Hash (Context, Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.Conduit.Binary (sinkFile, sourceFile)
import Data.List
import Data.Pool (Pool)
import Data.Text (pack, toLower)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data_Storage.Database_AccessControl (
    isUserTrustedOrContact,
 )
import Data_Storage.Model (ContentType (..), Document (..), DocumentId, EntityField (DocumentAccessUserId, DocumentId, DocumentPrivacyLevel, DocumentUserRef), PrivacyLevel (BlackBox, ConfidentialFirstParty, ConfidentialThirdParty, Private, Public, PublicLimited), UserId, documentAccessDocumentId)
import Database.Persist.Postgresql (Entity, selectList, (==.))
import Database.Persist.Sql (Entity (Entity, entityKey, entityVal), SqlBackend, SqlPersistT, get, getJust, getJustEntity, insert, runSqlPersistMPool, runSqlPool, (<-.))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Prelude hiding (FilePath)

type FilePath = String

storageBasePath :: FilePath
storageBasePath = "storage/documents"

-- | Determines the content type of a file based on its extension.
determineContentType :: FilePath -> ContentType
determineContentType filePath =
    let extension = toLower . pack $ takeExtension filePath
     in case extension of
            ".txt" -> Text
            ".jpeg" -> Photo
            ".jpg" -> Photo
            ".png" -> Photo
            ".gif" -> Photo
            ".mp4" -> Video
            ".avi" -> Video
            _ -> Other extension

-- Updated to include privacyLevel parameter
uploadDocument :: Pool SqlBackend -> UserId -> FilePath -> PrivacyLevel -> IO (Either Text DocumentId)
uploadDocument pool userId filePath privacyLevel = do
    currentTime <- getCurrentTime
    fileHash <- streamFileAndHash filePath
    let documentHash = hashToText fileHash
    let storagePath = documentStoragePath userId documentHash
    createDirectoryIfMissing True (takeDirectory storagePath)
    runConduitRes $ sourceFile filePath .| sinkFile storagePath
    let contentType = determineContentType filePath -- Function to determine content type based on file extension or MIME type
    let document = Document userId contentType documentHash privacyLevel currentTime
    docId <- runSqlPersistMPool (Database.Persist.Sql.insert document) pool
    return $ Right docId

-- Constructs the storage path for a document based on the user ID and document hash.
documentStoragePath :: UserId -> Text -> FilePath
documentStoragePath userId hashText =
    storageBasePath </> show userId </> toString hashText

-- Converts a SHA256 digest to a Text representation.
hashToText :: Digest SHA256 -> Text
hashToText = Data.Text.Encoding.decodeUtf8 . convertToBase Base16

-- Streams a file from disk and computes its SHA256 hash.
streamFileAndHash :: FilePath -> IO (Digest SHA256)
streamFileAndHash file = runConduitRes $ sourceFile file .| hashSink hashInit

-- Define a custom recursive function to process each chunk and update the context.
processChunks :: Context SHA256 -> ConduitT ByteString Void (ResourceT IO) (Context SHA256)
processChunks ctx = do
    maybeChunk <- await -- Attempt to read the next chunk
    case maybeChunk of
        Nothing -> return ctx -- No more data, return the final context
        Just chunk -> do
            let updatedCtx = hashUpdate ctx chunk -- Update the context with the current chunk
            processChunks updatedCtx -- Recursively process the next chunk

-- Conduit sink that consumes ByteStrings, updates the hash context, and finalizes the hash.
hashSink :: Context SHA256 -> ConduitT ByteString Void (ResourceT IO) (Digest SHA256)
hashSink ctx = do
    finalCtx <- processChunks ctx -- Process all chunks and get the final context
    return $ hashFinalize finalCtx -- Finalize and return the digest

-- Downloads a document if it exists, returning its contents or an error message.
downloadDocument :: Pool SqlBackend -> DocumentId -> IO (Either Text ByteString)
downloadDocument pool docId = do
    maybeDoc <- runSqlPersistMPool (Database.Persist.Sql.get docId) pool -- Correct usage of `get` from Database.Persist.Sql
    case maybeDoc of
        Just doc -> do
            let filePath = documentStoragePath (documentUserRef doc) (documentContent doc)
            fileExists <- doesFileExist filePath
            if fileExists then Right <$> readFileBS filePath else return $ Left "File does not exist."
        Nothing -> return $ Left "Document not found."

-- Lists all documents associated with a user.
listUserDocuments :: Pool SqlBackend -> UserId -> IO [Entity Document]
listUserDocuments pool userId =
    runSqlPersistMPool (selectList [DocumentUserRef ==. userId] []) pool

listAccessibleDocuments :: Pool SqlBackend -> UserId -> UserId -> IO [Entity Document]
listAccessibleDocuments pool accessingUserId documentOwnerId = runSqlPool action pool
  where
    action :: SqlPersistT IO [Entity Document]
    action = do
        -- Fetch documents based on visibility and relationship
        documentsVisibleBasedOnPrivacy <-
            selectList
                [ DocumentUserRef ==. documentOwnerId
                , DocumentPrivacyLevel <-. [Public, PublicLimited]
                    ++ if accessingUserId == documentOwnerId then [Private, ConfidentialFirstParty, ConfidentialThirdParty] else []
                ]
                []

        -- Fetch documents where access is explicitly granted through DocumentAccess
        explicitlyGrantedDocumentEntities <- selectList [DocumentAccessUserId ==. accessingUserId] []

        -- Extract document IDs from explicitly granted documents
        let explicitlyGrantedDocIds = map (documentAccessDocumentId . entityVal) explicitlyGrantedDocumentEntities

        -- Fetch documents for explicitly granted document IDs
        explicitlyGrantedDocuments <- mapM (\docId -> selectList [DocumentId ==. docId] []) explicitlyGrantedDocIds

        -- Combine and deduplicate documents
        let allDocuments = nubBy ((==) `on` entityKey) $ documentsVisibleBasedOnPrivacy ++ concat explicitlyGrantedDocuments

        return allDocuments