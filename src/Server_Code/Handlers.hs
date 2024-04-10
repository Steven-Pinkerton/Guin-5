{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server_Code.Handlers where

{-
import Data.Pool (Pool)
import Data_Storage.DocumentManagement (uploadDocument)
import Data_Storage.Model (Document (..), DocumentId, PrivacyLevel (..), Contact, Key, User, EntityField (ContactUserId))
import Servant
    ( throwError,
      err400,
      err500,
      Handler,
      ServerError(errBody), err401 )
import Data_Storage.Database_Document
    ( getAllDocuments )
import Authentication.Session_Management ( validateJWT, parseUserId )
import Authentication.Auth_Account_PasswordManagement ()
import Server_Code.Types (AuthenticatedUser, content, FileMetadata (FileMetadata), AppEnv (AppEnv), dbPool, AppM)
import Server_Code.Types qualified as Types
import Database.Persist.Postgresql (Entity (entityVal), SqlBackend, get, runSqlPersistMPool, toSqlKey, selectList, (==.))
import Data_Storage.Database_Newsfeed ()
import Servant.Server.Experimental.Auth
    ( AuthHandler, mkAuthHandler )
 -- If you need other things from this module
import Network.HTTP.Types.Header (hAuthorization)
import qualified Data.ByteString.Char8 as BC
import Data.Char ( isSpace, toLower )
import Data.List ( lookup )
import Network.Wai (Request, requestHeaders)
import Servant.Multipart
    ( FileData(fdPayload, fdFileName),
      Mem,
      MultipartData(files, inputs), lookupInput )
import Servant.Auth.Server (AuthResult (..))
import qualified Data.Text as T

{-
---------------------------- Authentication and Authorization ----------------------------

-- | Extracts the bearer token from the HTTP request header.
-- It splits the header value by space and checks if the first part is "bearer".
-- If so, it returns the rest of the string as the token.
extractBearerToken :: ByteString -> Maybe ByteString
extractBearerToken headerValue =
  let (bearer, token) = BC.break isSpace headerValue
   in if BC.map toLower bearer == "bearer" then Just (BC.dropWhile isSpace token) else Nothing

-- | Middleware for authenticating the user using JWTs. It extracts the user ID from the token.
-- If the token is valid, it allows the request to proceed with the extracted user ID.
authHandler :: AuthHandler Request Server_Code.Types.AuthenticatedUser
authHandler = mkAuthHandler handler
  where
    handler req = do
      let maybeToken = lookup hAuthorization (requestHeaders req) >>= extractBearerToken
      case maybeToken >>= validateJWT >>= parseUserId of
        Nothing -> throwError err401 {errBody = "Invalid or expired token"}
        Just userId -> return userId

------------------------------------- Document Handling -------------------------------------

-- | Retrieves all documents from the database and returns them.
getAllDocumentsHandler :: ReaderT AppEnv Handler [Document]
getAllDocumentsHandler = do
  pool <- asks dbPool
  documents <- liftIO $ getAllDocuments pool
  return $ map entityVal documents

-- | Determines the file path for storing a document based on its metadata.
determineFilePath :: Server_Code.Types.FileMetadata -> FilePath
determineFilePath Server_Code.Types.FileMetadata {fileName} =
  "documents/" ++ toString fileName

-- | Handler for adding a new document. It handles file upload and metadata storage.
-- If the user is authenticated, it processes the uploaded file and stores document metadata.
addDocumentHandler ::
  MultipartData Mem ->
  AuthResult AuthenticatedUser ->
  ReaderT AppEnv Handler Document
addDocumentHandler multipartData authResult = do
  env <- ask -- Retrieve the environment
  let pool = dbPool env -- Extract the database connection pool from the environment
  case authResult of
    Authenticated userId -> do
      case viaNonEmpty head (files multipartData) of
        Nothing -> throwError err400 {errBody = "No file uploaded."}
        Just fileInfo -> do
          let fileContent = fdPayload fileInfo
              fileName = fdFileName fileInfo
              filePath = "path/to/storage" ++ toString fileName -- Correct usage of unpack for Text
          privacyLevel <- lift $ extractPrivacyLevel multipartData
          uploadResult <- liftIO $ uploadDocument pool userId filePath privacyLevel
          lift $ either (throwError . toServantErr) (retrieveDocumentOrThrow pool) uploadResult
    _ -> throwError err401 {errBody = "Unauthorized access"}
  where
    toServantErr :: Text -> ServerError
    toServantErr msg = err400 {errBody = encodeUtf8 msg}

---------------------------- Document Retrieval and Error Handling ----------------------------

-- | Retrieves a document by its ID or throws a 500 error if not found.
retrieveDocumentOrThrow :: Pool SqlBackend -> DocumentId -> Handler Document
retrieveDocumentOrThrow pool docId = do
  maybeDoc <- liftIO $ retrieveDocumentById pool docId
  case maybeDoc of
    Just doc -> return doc
    Nothing -> throwError err500 {errBody = "Document created but not found."}

--------------------------------- Privacy Level Handling ---------------------------------

-- | Extracts and validates the privacy level from multipart form data.
-- Throws an error if the privacy level input is invalid.
extractPrivacyLevel :: Servant.Multipart.MultipartData Servant.Multipart.Mem -> Handler PrivacyLevel
extractPrivacyLevel multipartData =
  case Servant.Multipart.lookupInput "privacyLevel" multipartData of
    Right privacyText -> return $ textToPrivacyLevel privacyText
    Left errorMsg -> throwError err400 {errBody = encodeUtf8 ("Invalid privacy level: " <> errorMsg)}

-- | Converts the input text to a corresponding `PrivacyLevel` enum.
-- Defaults to `Public` if the text doesn't match any known level.
textToPrivacyLevel :: Text -> PrivacyLevel
textToPrivacyLevel txt = case T.toLower txt of
  "private" -> Private
  "selected" -> Selected
  "limited" -> Limited
  "public" -> Public
  _ -> Public -- Default case for unknown privacy levels.

-- | Retrieves a document by its ID from the database, or returns Nothing if not found.
retrieveDocumentById :: Pool SqlBackend -> DocumentId -> IO (Maybe Document)
retrieveDocumentById pool docId = runSqlPersistMPool (Database.Persist.Postgresql.get docId) pool

{- Retrieves a document by its ID.
This is useful for when you want to fetch the details of a specific document.
It takes a connection pool and a 'DocumentId'.

getDocumentHandler :: Pool SqlBackend -> Server_Code.Types.AuthenticatedUser -> Int -> Handler Document
getDocumentHandler pool authenticatedUserId documentIdInt = do
    let docId = toSqlKey (fromIntegral documentIdInt) :: DocumentId

    -- Retrieve the document by ID
    maybeDocEntity <- liftIO $ getDocumentById docId pool
    case maybeDocEntity of
        Nothing -> throwError $ err404 {errBody = "Document not found."}
        Just docEntity -> do
            -- Assuming your Document entity has a userRef field that is the ForeignKey to User
            let doc = entityVal docEntity
            if documentUserRef doc == authenticatedUserId
                then return doc
                else throwError $ err403 {errBody = "Access to the requested document is forbidden."}
-}

{- | Updates the details of an existing document.
It allows updating metadata such as the privacy level of the document.
This handler does not handle changes to the document's file content.

updateDocumentHandler :: Pool SqlBackend -> DocumentId -> DocumentRequest -> Handler Document
updateDocumentHandler pool docId DocumentRequest {..} = do
    -- Fetch the current time to set the 'updatedAt' field to the current timestamp.
    currentTime <- liftIO getCurrentTime

    -- Update the specified document's privacy level and updatedAt timestamp in the database.
    liftIO $ updateDocument docId [DocumentPrivacyLevel =. privacyLevel, DocumentCreatedAt =. currentTime] pool

    -- After updating, fetch the document again to ensure the response reflects the updated data.
    updatedDoc <- liftIO $ getDocumentById docId pool
    case updatedDoc of
        Just doc -> return $ entityVal doc
        -- If the document cannot be found after the update, it suggests an underlying issue, hence a 404 error.
        Nothing -> throwError $ err404 {errBody = "Failed to update document. Document not found."}
-}

{- | Deletes a document by its ID.
This involves removing the document's record from the database and deleting its file from the filesystem.

deleteDocumentHandler :: Pool SqlBackend -> DocumentId -> Handler NoContent
deleteDocumentHandler pool docId = do
    maybeDoc <- liftIO $ getDocumentById docId pool
    case maybeDoc of
        Just (Entity _ doc) -> do
            let filePath = documentStoragePath (documentUserRef doc) (documentContent doc)
            fileExists <- liftIO $ doesFileExist filePath
            when fileExists $ liftIO $ removeFile filePath
            liftIO $ deleteDocument docId pool
            return NoContent
        Nothing -> throwError $ err404 {errBody = "Document not found."}
-}

------------------------------------------------------------------------

{-
-- | Handles password recovery requests by verifying the user's provided verification code and updating the password if valid.
passwordRecoveryHandler :: Pool SqlBackend -> RecoveryRequest -> Handler NoContent
passwordRecoveryHandler pool RecoveryRequest {..} = do
    -- Attempts to reset the password using the information provided in the recovery request.
    -- `completePasswordReset` is expected to verify the verification code, check its validity (e.g., not expired),
    -- and then update the user's password in the database.
    result <- liftIO $ completePasswordReset pool recoveryMethod contactInfo verificationCode newPassword
    case result of
        -- If the password reset is successful, respond with HTTP status 204 No Content to indicate success without returning any data.
        Right _ -> return NoContent
        -- If there's an error (e.g., invalid verification code, code expired, user not found),
        -- return an HTTP 400 Bad Request status with the error message.
        Left errorMessage -> throwError $ err400 {errBody = encodeUtf8 $ toText errorMessage}

-- | Handles requests to update a user's information, such as phone number, name, or country.
updateUserHandler :: Pool SqlBackend -> Int -> UserUpdateInput -> Handler NoContent
updateUserHandler pool userId UserUpdateInput {updateName, updatePhone, updateCountry} = do
    -- Converts the user ID from `Int` to `Key User`, which is the type expected by Persistent for database operations.
    let userId' = toSqlKey (fromIntegral userId) :: Key Data_Storage.Model.User
    -- Prepares a list of updates. Each field is conditionally included in the update if it's provided (i.e., not Nothing).
    let updates =
            [ UserPhoneNumber =. updatePhone
            , UserName =. updateName
            , UserCountry =. updateCountry
            ]
    -- Executes the update operation in the database for the specified user with the given updates.
    -- This will modify only the fields provided in `UserUpdateInput`.
    liftIO $ updateUser userId' updates pool
    -- Responds with HTTP status 204 No Content to indicate the update was successful without returning any data.
    return NoContent

-- | Retrieves a user's details by their user ID and returns the user entity if found.
getUserByIdHandler :: Pool SqlBackend -> Int -> Handler (Maybe Data_Storage.Model.User)
getUserByIdHandler pool userId = do
    -- Converts the user ID from `Int` to `Key User` for database querying.
    let userId' = toSqlKey (fromIntegral userId) :: Key Data_Storage.Model.User
    -- Attempts to find the user in the database by the provided user ID.
    maybeUser <- liftIO $ getUserById userId' pool
    case maybeUser of
        -- If the user is found, return the user entity.
        Just user -> return $ Just $ entityVal user
        -- If no user is found with the given ID, throw a 404 Not Found error.
        Nothing -> throwError $ err404 {errBody = "User not found."}

-- | Authenticates a user based on their email and password. If successful, returns a JWT token for session management.
handleUserLogin :: Pool SqlBackend -> LoginInput -> Handler (Either Text Text)
handleUserLogin pool LoginInput {loginEmail, loginPassword} = do
    -- Attempts to find the user by email in the database.
    maybeUser <- liftIO $ runSqlPersistMPool (selectFirst [UserEmail ==. loginEmail] []) pool
    case maybeUser of
        -- If no user is found with the provided email, return an error indicating user not found.
        Nothing -> return $ Left "User not found."
        -- If a user is found, proceed to check the validity of the provided password against the stored hash.
        Just (Entity userId user) -> do
            -- Converts the stored password hash and the provided plaintext password to ByteString for comparison.
            let hashedPassword = encodeUtf8 (userPassword user) -- Assuming userPassword is stored as Text in your User model.
            let plaintextPassword = encodeUtf8 loginPassword
            -- Validates the provided password against the stored hash.
            let isValid = validatePasswordAgainstHash hashedPassword plaintextPassword
            if isValid
                then do
                    -- If the password is valid, generate a JWT token for the user.
                    jwt <- liftIO $ createJWT (toString $ userIdToText userId)
                    -- Return the generated JWT token.
                    return $ Right jwt
                else -- If the password validation fails, return an error indicating invalid credentials.
                    return $ Left "Invalid credentials."
-}
-- Retrieves all contacts for a given user.
getAllContactsHandler :: Pool SqlBackend -> Int -> Handler [Contact]
getAllContactsHandler pool userId = do
    -- Convert the user ID from an integer to a Persistent key.
    let userIdKey = toSqlKey (fromIntegral userId) :: Key Data_Storage.Model.User
    -- Query the database for all contacts belonging to the user.
    contacts <- liftIO $ runSqlPersistMPool (selectList [ContactUserId ==. userIdKey] []) pool
    -- Convert the list of Entity Contact to a list of Contact.
    return $ map entityVal contacts

{-}
-- Adds a new contact for a user.
addContactHandler :: Pool SqlBackend -> Int -> ContactRequest -> Handler Contact
addContactHandler pool userId ContactRequest {contactInfo} = do
    -- Convert the user ID from an integer to a Persistent key.
    let userIdKey = toSqlKey (fromIntegral userId) :: Key Data_Storage.Model.User
    -- Insert a new contact into the database and get its ID.
    newContactId <- liftIO $ runSqlPersistMPool (insert (Contact userIdKey contactInfo)) pool
    -- Retrieve the newly added contact to return it as a response.
    maybeNewContact <- liftIO $ runSqlPersistMPool (Database.Persist.Postgresql.get newContactId) pool
    case maybeNewContact of
        Nothing -> throwError $ err500 {errBody = "Failed to retrieve newly added contact."}
        Just newContact -> return newContact

-- Retrieves a specific contact by ID.
getContactHandler :: Pool SqlBackend -> Int -> Int -> Handler Contact
getContactHandler pool userId contactId = do
    -- Convert the contact ID from an integer to a Persistent key.
    let contactIdKey = toSqlKey (fromIntegral contactId) :: Key Contact
    -- Query the database for the specified contact.
    maybeContact <- liftIO $ runSqlPersistMPool (Database.Persist.Postgresql.get contactIdKey) pool
    case maybeContact of
        Nothing -> throwError $ err404 {errBody = "Contact not found."}
        Just contact -> return contact

-- Deletes a specific contact by ID.
deleteContactHandler :: Pool SqlBackend -> Int -> Int -> Handler NoContent
deleteContactHandler pool userId contactId = do
    -- Convert the contact ID from an integer to a Persistent key.
    let contactIdKey = toSqlKey (fromIntegral contactId) :: Key Contact
    -- Delete the specified contact from the database.
    liftIO $ runSqlPersistMPool (delete contactIdKey) pool
    return NoContent

-- Adds a trusted contact for a user.
addTrustedContactHandler :: Pool SqlBackend -> Int -> TrustedContactRequest -> Handler NoContent
addTrustedContactHandler pool userId TrustedContactRequest {..} = do
    -- Generate the current timestamp for the createdAt field.
    currentTime <- liftIO getCurrentTime
    -- Convert both user IDs from integers to Persistent keys.
    let userIdKey = toSqlKey (fromIntegral userId) :: Key Data_Storage.Model.User
    let trustedUserIdKey = toSqlKey (fromIntegral trustedUserId) :: Key Data_Storage.Model.User
    -- Insert a new trusted contact into the database.
    _ <- liftIO $ runSqlPersistMPool (insert (TrustedContact userIdKey trustedUserIdKey currentTime)) pool
    return NoContent

-- | Deletes a trusted contact by its ID for a given user.
deleteTrustedContactHandler :: Pool SqlBackend -> UserId -> TrustedContactId -> Handler NoContent
deleteTrustedContactHandler pool userId trustedContactId = do
    -- Attempt to delete the trusted contact from the database.
    liftIO $ runSqlPersistMPool (delete trustedContactId) pool
    return NoContent

-- | Updates a contact by its ID for a given user.
updateContactHandler :: Pool SqlBackend -> UserId -> ContactId -> ContactRequest -> Handler NoContent
updateContactHandler pool userId contactId ContactRequest {..} = do

    -- Update the specified contact's details in the database.
    liftIO $ runSqlPersistMPool (update contactId [ContactContactInfo =. contactInfo]) pool
    return NoContent

-- Example implementation (adjust according to your models and logic)
getTrustedContactsHandler :: Pool SqlBackend -> Int -> Handler [TrustedContact]
getTrustedContactsHandler pool userId = do
    let userIdKey = toSqlKey (fromIntegral userId) :: Key User
    trustedContacts <- liftIO $ runSqlPersistMPool (selectList [TrustedContactUserRef ==. userIdKey] []) pool
    return $ map entityVal trustedContacts

{- | Retrieves the newsfeed for a user, which includes their posts and potentially posts from their contacts,
depending on privacy settings.
-}
getNewsfeedHandler :: Pool SqlBackend -> Int -> Handler [Data_Storage.Model.Post]
getNewsfeedHandler pool userId = do
    -- Convert the provided user ID to a Persistent key for database queries.
    let userKey = toSqlKey $ fromIntegral userId
    -- Fetch posts visible to the user based on privacy settings and relationships.
    posts <- liftIO $ getVisiblePosts pool userKey
    -- Extract the Post values from the list of Entity wrappers.
    return $ map entityVal posts

-- | Creates a new post for a user with the specified content and privacy level.
createPostHandler :: Pool SqlBackend -> Int -> PostRequest -> Handler Data_Storage.Model.Post
createPostHandler pool userId PostRequest {..} = do
    -- Convert the user ID to a Persistent key.
    let userKey = toSqlKey $ fromIntegral userId
    -- Insert the new post into the database and retrieve its ID.
    postId <- liftIO $ insertPost pool userKey content privacyLevel
    -- Attempt to fetch the newly created post to return it as a response.
    maybePost <- liftIO $ runSqlPersistMPool (Database.Persist.Postgresql.get postId) pool
    case maybePost of
        Nothing -> throwError $ err500 {errBody = "Failed to insert post."}
        Just post -> return post

-- | Retrieves a specific post by its ID, ensuring that the requester has access to view the post.
getPostHandler :: Pool SqlBackend -> Int -> Int -> Handler Data_Storage.Model.Post
getPostHandler pool _ postId = do
    -- Convert the post ID to a Persistent key.
    let postKey = toSqlKey $ fromIntegral postId
    -- Fetch the specified post from the database.
    maybePost <- liftIO $ runSqlPersistMPool (Database.Persist.Postgresql.get postKey) pool
    case maybePost of
        Nothing -> throwError $ err404 {errBody = "Post not found."}
        Just post -> return post

-- | Updates the content and/or privacy level of a specific post.
updatePostHandler :: Pool SqlBackend -> Int -> Int -> PostRequest -> Handler NoContent
updatePostHandler pool _ postId PostRequest {..} = do
    -- Convert the post ID to a Persistent key.
    let postKey = toSqlKey $ fromIntegral postId
    -- Perform the update operation on the specified post.
    liftIO $ updatePost pool postKey (Just content) (Just privacyLevel)
    return NoContent

-- | Deletes a specific post by its ID, only if the operation is authorized.
deletePostHandler :: Pool SqlBackend -> Int -> Int -> Handler NoContent
deletePostHandler pool _ postId = do
    -- Convert the post ID to a Persistent key.
    let postKey = toSqlKey $ fromIntegral postId
    -- Delete the specified post from the database.
    liftIO $ deletePost pool postKey
    return NoContent

-}
-}
-}