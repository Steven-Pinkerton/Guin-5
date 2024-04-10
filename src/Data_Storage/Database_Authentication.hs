module Data_Storage.Database_Authentication where

import Data.Pool (Pool)
import Data_Storage.Database_Connection (withDbConnection)
import Data_Storage.Model qualified as Model
import Database.Persist.Postgresql (delete, insert, selectFirst, update, (==.))
import Database.Persist.Sql (Entity, SqlBackend, Update)

-- Insert new authentication data into the database
insertAuthData :: Model.Authentication -> Pool SqlBackend -> IO Model.AuthenticationId
insertAuthData authData = withDbConnection (insert authData)

-- Retrieve authentication data by ID
getAuthDataById :: Model.AuthenticationId -> Pool SqlBackend -> IO (Maybe (Entity Model.Authentication))
getAuthDataById authId = withDbConnection (selectFirst [Model.AuthenticationId ==. authId] [])

-- Update authentication data
updateAuthData :: Model.AuthenticationId -> [Update Model.Authentication] -> Pool SqlBackend -> IO ()
updateAuthData authId updates = withDbConnection (update authId updates)

-- Delete authentication data by ID
deleteAuthData :: Model.AuthenticationId -> Pool SqlBackend -> IO ()
deleteAuthData authId = withDbConnection (delete authId)
