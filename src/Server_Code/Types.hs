module Server_Code.Types where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Time ()
import Data_Storage.Model (Document (..), DocumentId, PrivacyLevel, UserId)
import Database.Persist.Postgresql (SqlBackend)
import Servant (AuthProtect, Handler)
import Servant.Server.Experimental.Auth (AuthServerData)

type AuthenticatedUser = Data_Storage.Model.UserId

type instance AuthServerData (AuthProtect "jwt") = AuthenticatedUser

data AppEnv = AppEnv
    { dbPool :: Pool SqlBackend
    }

type AppM = ReaderT AppEnv Handler

data FileMetadata = FileMetadata
    { fileName :: Text
    , contentType :: Text -- MIME type
    }

-- Define a data type for the document creation and update request

data DocumentRequest = DocumentRequest
    { fileData :: ByteString
    , fileMetadata :: FileMetadata
    , privacyLevel :: PrivacyLevel
    }

-- | Define a type for the user registration input, mirroring what's necessary for `registerUser`.
data RegistrationInput = RegistrationInput
    { email :: Text
    , password :: Text
    , phoneNumber :: Maybe Text
    , name :: Maybe Text
    , dob :: Maybe Text -- Date of birth as Text, to be converted server-side
    , country :: Maybe Text
    -- Add other fields as necessary
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON RegistrationInput
instance ToJSON RegistrationInput

data RecoveryMethod = Email | SMS deriving stock (Eq, Show, Generic)
instance FromJSON RecoveryMethod
instance ToJSON RecoveryMethod

data RecoveryRequest = RecoveryRequest
    { recoveryMethod :: RecoveryMethod
    , contactInfo :: Text -- This can be either an email or phone number depending on the method
    , verificationCode :: Text
    , newPassword :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON RecoveryRequest
instance ToJSON RecoveryRequest

data UserUpdateInput = UserUpdateInput
    { updateName :: Maybe Text
    , updatePhone :: Maybe Text
    , updateCountry :: Maybe Text
    -- Add other fields as necessary
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON UserUpdateInput
instance ToJSON UserUpdateInput

data LoginInput = LoginInput
    { loginEmail :: Text
    , loginPassword :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON LoginInput
instance ToJSON LoginInput

data ContactRequest = ContactRequest
    { contactInfo :: Text
    , contactLabel :: Maybe Text -- Optional label or name for the contact
    , contactType :: Maybe Text -- Optional relationship or type
    -- Consider adding more fields as necessary
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ContactRequest
instance ToJSON ContactRequest

newtype TrustedContactRequest = TrustedContactRequest
    { trustedUserId :: Int
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON TrustedContactRequest
instance ToJSON TrustedContactRequest

data PostRequest = PostRequest
    { content :: Text
    , privacyLevel :: PrivacyLevel
    }
    deriving stock (Eq, Show, Generic)
