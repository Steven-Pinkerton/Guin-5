{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data_Storage.Model where

import Data.Aeson (
    FromJSON (parseJSON),
    Options (constructorTagModifier),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
 )
import Data.Char (isLower, toLower)
import Data.Time (UTCTime)
import Database.Persist.TH (
    derivePersistField,
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
 )

-- Profile Card (everyone has one); a card is what other users see when they look at your profile if yoru in the network.
-- Persona Cards (optional) as many as you want. For alter-ego's or different personas. Another multiple profile; each persona card has its own masking hash
-- Forum Setting; showing you who of your contacts are online.
-- Market setting; list of persona's that you follow.
-- Profiles and Personn's need options for Themes; customisation of the profile. Colour, Banner and Backround.
-- So you'll have contract list; forum, market, personas that are yours (Sign you for each persona)
-- Define the PrivacyLevel outside the share block
-- Blackbox (Private) - Only the user can see the data; fully encrypted; may or may not requie key to access
-- Confidental (First Party) - User created the document and can select who sees it.
-- Confidential (Third Party) - Documents created by third party that user has acess to. (Don't own doucment but can access/see it, the status for those who have access to the document)
-- Public Limited - Anyone who is a contact of a user can see.
-- Public Unlimited - Your network can see it, and they cna share it with their network.
-- Default privacy level should be blackbox
-- Anything that is public or limited will show up on persona cards.
data PrivacyLevel = Private | Limited | Public | BlackBox | ConfidentialFirstParty | ConfidentialThirdParty | PublicLimited | PublicUnlimited deriving stock (Show, Read, Eq, Generic)
derivePersistField "PrivacyLevel"

instance FromJSON PrivacyLevel where
    parseJSON = genericParseJSON defaultOptions{constructorTagModifier = map toLower . dropWhile isLower}

instance ToJSON PrivacyLevel where
    toJSON = genericToJSON defaultOptions{constructorTagModifier = map toLower . dropWhile isLower}

data ContentType = Text | Video | Photo | Other Text
    deriving stock (Show, Read, Eq, Generic)

instance FromJSON ContentType where
    parseJSON = genericParseJSON defaultOptions{constructorTagModifier = camelTo2 '-'}

instance ToJSON ContentType where
    toJSON = genericToJSON defaultOptions{constructorTagModifier = camelTo2 '-'}

derivePersistField "ContentType"

-- security key; optional (up to three); not for prototype
-- emergency biometrics (optional); not for prototype
-- Users should have backup emails; optional
-- Authentation Questions they have selected (Two-Three Questions) for recvoery purposes; not optional mandatory
-- Driving license/passport/bank/utility account.
-- User hash and masking hask for masking a profile (Random alpha numeric) when a third party wants to verify your identiy; they would input the userhash you've shared
-- the system would throw up a random hash of that alphanumeric a masking hash. (Maybe a list of 10-20 hashes that are randomly generated and stored in the database associated with each users user hash))
-- GO to doctor, doctor wants to attach documents to my profile; asks for user hash; system won't allow him to connect things to me without my name and hash.
-- Trusted Users are for things like emergency contacts; each user can have five. These would have to be verified the same way you verify a user hash.
-- When a person wants to be verified as a trusted user; that person fristly input my users hash; then the system will send a message to me and ask me to input that persons user's hash. Only then will it verify them as a trusted contact.
-- Everything fades after 90 days; texts/posts the only thing that doesn't fade is your libary of documents.
-- Libary size is 1GB; if you want more you have to pay for it.
-- Libary's contact is documents
-- Privacy on all posts is public limited or public unlimited.
-- People can message persona regardless of if they are in there contact list or not. Persona's are public unlimited by default. Profiles are public limited by default.
-- Newsfeed is different to messaging system. The newsfeed has to be done through a profile.
-- User messaging and channel messaging (groups), a channel can have multiple people in it.

--UUID for each persona; each persona associated with a User' this is the "MaskingHash"; each user also has a hash.
-- Some form of bilateral verification for trusted contacts; a trusted contact has to be verified by the user and the user has to be verified by the trusted contact.
-- This can be done via OTP; the masking hash that will change every six minutes.
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

-- Users represent the core accounts in the system
User
    email Text
    backupEmail Text Maybe -- Optional backup email for recovery purposes
    phoneNumber Text Maybe
    password Text
    name Text Maybe
    dob UTCTime Maybe
    country Text Maybe
    photoUrl Text Maybe
    created_at UTCTime default='CURRENT_TIMESTAMP'
    updated_at UTCTime default='CURRENT_TIMESTAMP'
    UniqueEmail email
    UniquePhoneNumber phoneNumber !force
    deriving Show

-- Contact info for users
Contact
    userId UserId
    contactInfo Text
    deriving Show

-- Documents associated with users, including a privacy level
Document
    userRef UserId
    contentType ContentType
    content Text -- This could be actual text content or a URL to the content stored elsewhere
    privacyLevel PrivacyLevel
    createdAt UTCTime default='CURRENT_TIMESTAMP'
    deriving Show

-- Trusted contacts for emergency purposes or close associations
TrustedContact
    userRef UserId
    trustedUserId UserId
    createdAt UTCTime default='CURRENT_TIMESTAMP'
    UniqueUserTrust userRef trustedUserId
    deriving Show

-- Authentication methods for users, e.g., password, biometrics
Authentication
    userId UserId
    method Text
    credential Text
    createdAt UTCTime default='CURRENT_TIMESTAMP'
    UniqueAuthentication userId method
    deriving Show

-- Codes for account recovery
RecoveryCode
    userId UserId
    code Text
    expiresAt UTCTime
    UniqueRecoveryCode userId code
    deriving Show

-- Posts made by users
Post
    userId UserId
    content Text Maybe
    documentId DocumentId Maybe
    privacyLevel PrivacyLevel Default='Public'
    created_at UTCTime default='CURRENT_TIMESTAMP'
    updated_at UTCTime default='CURRENT_TIMESTAMP'
    deriving Show

-- Viewers of posts, for controlling access
PostViewer
    postId PostId
    viewerId UserId
    UniquePostViewer postId viewerId
    deriving Show

-- New: Profile and Persona Cards for users
ProfileCard
    userId UserId
    themeId ThemeId -- Foreign key to a Theme
    privacyLevel PrivacyLevel Default='PublicLimited'
    deriving Show

Persona
    userId UserId
    personaHash Text -- Unique hash for the persona
    themeId ThemeId -- Foreign key to a Theme
    privacyLevel PrivacyLevel Default='PublicUnlimited'
    UniquePersonaHash personaHash
    deriving Show

-- New: Themes for customizing profile and persona cards
Theme
    color Text
    banner Text
    background Text
    deriving Show

-- New: Authentication questions for user account recovery
AuthQuestion
    userId UserId
    question Text
    answer Text
    deriving Show

-- New: Library for storing user documents
Library
    userId UserId
    sizeBytes Int default=0 -- Size of documents stored in bytes
    capacityBytes Int default=1073741824 -- 1GB capacity by default
    deriving Show

MaskingHash
    userId UserId -- Adjust the field name to match the database column name
    hash Text
    createdAt UTCTime default='CURRENT_TIMESTAMP'
    UniqueHash hash
    deriving Show

UserHash
    userId UserId
    hash Text
    createdAt UTCTime default='CURRENT_TIMESTAMP'
    UniqueUserHash hash
    deriving Show

DocumentAccess
    documentId DocumentId
    userId UserId
    grantedAt UTCTime
    deriving Show

-- Add any additional entities or modifications as needed based on project requirements
|]
