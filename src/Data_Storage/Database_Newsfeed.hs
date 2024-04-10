module Data_Storage.Database_Newsfeed where

import Data.Pool (Pool)

import Control.Monad.Logger
import Data.Text (pack)
import Data.Time (getCurrentTime)
import Data_Storage.Model (
    Contact (contactUserId),
    DocumentId,
    EntityField (
        ContactUserId,
        PostContent,
        PostPrivacyLevel,
        PostUserId
    ),
    Key,
    Post (Post),
    PostId,
    PrivacyLevel (PublicLimited, PublicUnlimited),
    UserId,
    postContent,
    postPrivacyLevel,
 )
import Database.Persist (
    Entity (entityKey, entityVal),
    PersistStoreWrite (delete, insert, update),
    selectList,
    (<-.),
    (=.),
    (==.),
 )
import Database.Persist.Postgresql (
    SqlBackend,
    runSqlPersistMPool,
 )

-- Inserts a new post into the database
insertPost :: Pool SqlBackend -> UserId -> Maybe Text -> Maybe DocumentId -> PrivacyLevel -> IO (Key Post)
insertPost pool userId content documentId privacyLevel = liftIO $ do
    currentTime <- getCurrentTime
    let newPost = Post userId content documentId privacyLevel currentTime currentTime
    runSqlPersistMPool (insert newPost) pool

-- Retrieves contact user IDs for a given user
getContactUserIds :: Pool SqlBackend -> UserId -> IO [UserId]
getContactUserIds pool userId = liftIO $ do
    contacts <- runSqlPersistMPool (selectList [ContactUserId ==. userId] []) pool
    return $ map (contactUserId . entityVal) contacts

{- Retrieves posts visible to a specific user
getVisiblePosts :: Pool SqlBackend -> UserId -> LoggingT IO [Entity Post]
getVisiblePosts pool userId = do
  contactIds <- liftIO $ getContactUserIds pool userId
  logInfoN $ "Contact IDs: " <> pack (show contactIds)

  -- Dynamically construct filters based on contact IDs.
  let publicUnlimitedFilter = [PostPrivacyLevel ==. PublicUnlimited]
  let publicLimitedFilter =
        if not (null contactIds)
          then [PostPrivacyLevel ==. PublicLimited, PostUserId <-. contactIds]
          else []

  -- Combine filters for public posts and posts visible through contacts.
  let filters = publicUnlimitedFilter ++ publicLimitedFilter

  -- Execute query with constructed filters.
  posts <- liftIO $ runSqlPersistMPool (selectList filters []) pool
  logInfoN $ "Fetched posts count: " <> pack (show (length posts))
  return posts
-}

getPublicUnlimitedPosts :: Pool SqlBackend -> LoggingT IO [Entity Post]
getPublicUnlimitedPosts pool = do
    posts <- liftIO $ runSqlPersistMPool (selectList [PostPrivacyLevel ==. PublicUnlimited] []) pool
    logInfoN $ "Fetched PublicUnlimited posts count: " <> pack (show (length posts))
    return posts

getPublicLimitedPosts :: Pool SqlBackend -> UserId -> LoggingT IO [Entity Post]
getPublicLimitedPosts pool userId = do
    contactIds <- liftIO $ getContactUserIds pool userId -- Fetching contacts of the user
    logInfoN $ "User's contact IDs: " <> pack (show contactIds)

    -- Fetch PublicLimited posts visible to the user through their contacts
    let filters = [PostPrivacyLevel ==. PublicLimited, PostUserId <-. contactIds]

    posts <- liftIO $ runSqlPersistMPool (selectList filters []) pool
    logInfoN $ "Fetched PublicLimited posts count: " <> pack (show (length posts))
    return posts

getVisiblePosts :: Pool SqlBackend -> UserId -> LoggingT IO [Entity Post]
getVisiblePosts pool userId = do
    publicUnlimitedPosts <- getPublicUnlimitedPosts pool
    publicLimitedPosts <- getPublicLimitedPosts pool userId

    let allPosts = publicUnlimitedPosts ++ publicLimitedPosts

    -- Debugging output to inspect fetched posts
    mapM_
        ( \post ->
            logInfoN $
                "Post ID: " <> pack (show (entityKey post))
                    <> ", Content: "
                    <> (fromMaybe (pack "No content") (postContent . entityVal $ post))
                    <> ", Visibility: "
                    <> pack (show (postPrivacyLevel . entityVal $ post))
        )
        allPosts

    logInfoN $ "Total fetched posts count: " <> pack (show (length allPosts))
    return allPosts

-- Updates a post's content or privacy level
updatePost :: Pool SqlBackend -> PostId -> Maybe Text -> Maybe PrivacyLevel -> IO ()
updatePost pool postId newContent newPrivacyLevel = liftIO $ do
    let updates =
            catMaybes
                [ Just (PostContent =. newContent) -- Directly use the newContent, even if it's Nothing
                , fmap (PostPrivacyLevel =.) newPrivacyLevel -- This is correct because we're applying fmap over Maybe PrivacyLevel
                ]
    runSqlPersistMPool (update postId updates) pool

-- Deletes a post by its ID
deletePost :: Pool SqlBackend -> PostId -> IO ()
deletePost pool postId = liftIO $ runSqlPersistMPool (delete postId) pool