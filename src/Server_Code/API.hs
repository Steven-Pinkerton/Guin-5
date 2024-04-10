module Server_Code.API where

import Data.Aeson ()
import Data_Storage.Model (Contact, Document, Post, TrustedContact, User)
import Servant.API (
    AuthProtect,
    Capture,
    DeleteNoContent,
    Get,
    JSON,
    Post,
    PostNoContent,
    Put,
    PutNoContent,
    ReqBody,
    type (:<|>),
    type (:>),
 )
import Servant.Multipart (Mem, MultipartData, MultipartForm)
import Server_Code.Types (
    ContactRequest,
    DocumentRequest,
    LoginInput,
    PostRequest,
    RecoveryRequest,
    RegistrationInput,
    TrustedContactRequest,
    UserUpdateInput,
 )

type DocumentAPI =
    "documents" :> MultipartForm Mem (MultipartData Mem) :> AuthProtect "jwt" :> Servant.API.Post '[JSON] Document -- Add a new document
    -- :<|> "documents" :> Capture "documentId" Int :> AuthProtect "jwt" :> Get '[JSON] Document -- Retrieve a specific document by ID
    --  :<|> "documents" :> Capture "documentId" Int :> ReqBody '[JSON] DocumentRequest :> Put '[JSON] Document -- Update a specific document
    --   :<|> "documents" :> Capture "documentId" Int :> DeleteNoContent -- Remove a specific document
    --   :<|> "documents" :> Get '[JSON] [Document] -- Retrieve all documents

{- | User management endpoints
 type UserAPI =
  "users" :> "register" :> ReqBody '[JSON] RegistrationInput :> Servant.API.Post '[JSON] (Either Text Text) -- User registration NEED TO IMPLEMENT
   :<|> "users" :> "login" :> ReqBody '[JSON] LoginInput :> Servant.API.Post '[JSON] (Either Text Text) -- User login
  :<|> "users" :> Capture "userId" Int :> Get '[JSON] (Maybe User) -- Get user by ID
  :<|> "users" :> Capture "userId" Int :> ReqBody '[JSON] UserUpdateInput :> PutNoContent -- Update user information
  :<|> "users" :> "recover" :> ReqBody '[JSON] RecoveryRequest :> PostNoContent -- Password recovery DONE
 Add more endpoints as necessary
-}
type ContactAPI =
    "users" :> Capture "userId" Int :> "contacts" :> Get '[JSON] [Contact] -- Retrieve all contacts for a user
    -- :<|> "users" :> Capture "userId" Int :> "contacts" :> ReqBody '[JSON] ContactRequest :> Servant.API.Post '[JSON] Contact -- Add a new contact
    --  :<|> "users" :> Capture "userId" Int :> "contacts" :> Capture "contactId" Int :> Get '[JSON] Contact -- Retrieve a specific contact by ID
    -- :<|> "users" :> Capture "userId" Int :> "contacts" :> Capture "contactId" Int :> ReqBody '[JSON] ContactRequest :> PutNoContent -- Update a specific contact
    --  :<|> "users" :> Capture "userId" Int :> "contacts" :> Capture "contactId" Int :> DeleteNoContent -- Remove a specific contact

-- type TrustedContactAPI =
--  "users" :> Capture "userId" Int :> "trustedContacts" :> Get '[JSON] [TrustedContact] -- Get all trusted contacts for a user
--   :<|> "users" :> Capture "userId" Int :> "trustedContacts" :> ReqBody '[JSON] TrustedContactRequest :> PostNoContent -- Add a new trusted contact
--   :<|> "users" :> Capture "userId" Int :> "trustedContacts" :> Capture "trustedContactId" Int :> DeleteNoContent -- Remove a trusted contact

-- type NewsfeedAPI =
-- "users" :> Capture "userId" Int :> "newsfeed" :> Get '[JSON] [Data_Storage.Model.Post] -- Get newsfeed for a user
--   :<|> "users" :> Capture "userId" Int :> "posts" :> ReqBody '[JSON] PostRequest :> Servant.API.Post '[JSON] Data_Storage.Model.Post -- Create a new post
--  :<|> "users" :> Capture "userId" Int :> "posts" :> Capture "postId" Int :> Get '[JSON] Data_Storage.Model.Post -- Retrieve a specific post by ID
--  :<|> "users" :> Capture "userId" Int :> "posts" :> Capture "postId" Int :> ReqBody '[JSON] PostRequest :> PutNoContent -- Update a specific post
-- :<|> "users" :> Capture "userId" Int :> "posts" :> Capture "postId" Int :> DeleteNoContent -- Delete a specific post

-- You might also want to define additional types or APIs for other functionalities or details.
