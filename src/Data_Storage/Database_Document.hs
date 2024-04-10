module Data_Storage.Database_Document where

import Data.Pool (Pool)
import Data_Storage.Database_Connection (withDbConnection)
import Data_Storage.Model qualified as Model
import Database.Persist.Postgresql (Update, delete, insert, selectFirst, selectList, update, (==.))
import Database.Persist.Sql (Entity, SqlBackend, runSqlPersistMPool)

-- Insert a new document into the database
insertDocument :: Model.Document -> Pool SqlBackend -> IO (Model.Key Model.Document)
insertDocument document = withDbConnection (insert document)

-- Retrieve a document by ID
getDocumentById :: Model.DocumentId -> Pool SqlBackend -> IO (Maybe (Entity Model.Document))
getDocumentById documentId = withDbConnection (selectFirst [Model.DocumentId ==. documentId] [])

-- Retrieve documents for a specific user
getDocumentsByUserId :: Model.UserId -> Pool SqlBackend -> IO [Entity Model.Document]
getDocumentsByUserId userId = withDbConnection (selectList [Model.DocumentUserRef ==. userId] [])

-- Update a document's information
updateDocument :: Model.DocumentId -> [Update Model.Document] -> Pool SqlBackend -> IO ()
updateDocument documentId updates = withDbConnection (update documentId updates)

-- Delete a document by ID
deleteDocument :: Model.DocumentId -> Pool SqlBackend -> IO ()
deleteDocument documentId = withDbConnection (delete documentId)

getAllDocuments :: Pool SqlBackend -> IO [Entity Model.Document]
getAllDocuments = runSqlPersistMPool (selectList [] [])
