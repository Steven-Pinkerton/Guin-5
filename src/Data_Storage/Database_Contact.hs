module Data_Storage.Database_Contact where

import Data.Pool (Pool)
import Data_Storage.Database_Connection (withDbConnection)
import Data_Storage.Model qualified as Model
import Database.Persist.Postgresql (Update, delete, insert, selectFirst, selectList, update, (==.))
import Database.Persist.Sql (Entity, SqlBackend)

-- Insert a new contact into the database
insertContact :: Model.Contact -> Pool SqlBackend -> IO (Model.Key Model.Contact)
insertContact contact = withDbConnection (insert contact)

-- Retrieve a contact by ID
getContactById :: Model.ContactId -> Pool SqlBackend -> IO (Maybe (Entity Model.Contact))
getContactById contactId = withDbConnection (selectFirst [Model.ContactId ==. contactId] [])

-- Retrieve contacts for a specific user
getContactsByUserId :: Model.UserId -> Pool SqlBackend -> IO [Entity Model.Contact]
getContactsByUserId userId = withDbConnection (selectList [Model.ContactUserId ==. userId] [])

-- Update a contact's information
updateContact :: Model.ContactId -> [Update Model.Contact] -> Pool SqlBackend -> IO ()
updateContact contactId updates = withDbConnection (update contactId updates)

-- Delete a contact by ID
deleteContact :: Model.ContactId -> Pool SqlBackend -> IO ()
deleteContact contactId = withDbConnection (delete contactId)
