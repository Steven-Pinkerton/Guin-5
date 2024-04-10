{-# LANGUAGE OverloadedStrings #-}

module NewsFeedSpec where

-- Import necessary libraries and modules
import Data.Pool (Pool)
import Database.Persist (Filter, PersistQueryWrite (deleteWhere), PersistStoreWrite (insert), get)
import Database.Persist.Postgresql (SqlBackend, runSqlPersistMPool)
import Test.Hspec (
    Expectation,
    Spec,
    SpecWith,
    afterAll,
    beforeAll,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
 )

-- Custom modules for database and test utilities

import Control.Monad.Logger
import Data.Time
import Data_Storage.Database_Newsfeed (deletePost, getVisiblePosts, insertPost, updatePost)
import Data_Storage.Model (Contact, Post (Post, postContent), PostId, PrivacyLevel (PublicLimited, PublicUnlimited), User, UserId)
import TestDatabaseCode (createTestDatabasePool)
import TestUtils (addTestContact, createTestUser)
import Prelude hiding (get)

-- Setup and teardown actions for the test environment
setupEnv :: IO (Pool SqlBackend, UserId)
setupEnv = do
    putStrLn "Setting up test environment..."
    pool <- createTestDatabasePool
    userId <- createTestUser pool
    putStrLn $ "Test environment setup complete with UserId: " ++ show userId
    return (pool, userId)

cleanEnv :: (Pool SqlBackend, UserId) -> IO ()
cleanEnv (pool, _) = do
    putStrLn "Cleaning up test environment..."
    runSqlPersistMPool
        ( do
            deleteWhere ([] :: [Filter Contact])
            deleteWhere ([] :: [Filter Post])
            deleteWhere ([] :: [Filter User])
        )
        pool
    putStrLn "Test environment cleanup complete."

-- Main spec function that organizes all test cases
newsFeedspec :: Spec
newsFeedspec = beforeAll setupEnv $
    afterAll cleanEnv $ do
        newsfeedFunctionalitySpec
        deletePostSpec

-- Spec for testing newsfeed functionality
newsfeedFunctionalitySpec :: SpecWith (Pool SqlBackend, UserId)
newsfeedFunctionalitySpec = describe "Newsfeed functionality" $ do
    it "inserts a new post into the database" testInsertPost
    it "retrieves visible posts for a user" testRetrieveVisiblePosts
    it "updates a post's content" testUpdatePostContent

-- Spec for testing the deletion of a post
deletePostSpec :: SpecWith (Pool SqlBackend, UserId)
deletePostSpec = describe "Deleting a post" $ do
    it "deletes a post by ID" testDeletePostById

-- Helper function to add a test post
addTestPost :: Pool SqlBackend -> UserId -> Maybe Text -> PrivacyLevel -> IO PostId
addTestPost pool userId content privacyLevel = do
    currentTime <- getCurrentTime
    let newPost = Post userId content Nothing privacyLevel currentTime currentTime -- Use Nothing for documentId
    postId <- runSqlPersistMPool (insert newPost) pool
    -- Adding logging
    putStrLn $ "Inserted Post with visibility: " <> show privacyLevel <> ", ID: " <> show postId
    return postId

-- Test case for inserting a post
testInsertPost :: (Pool SqlBackend, UserId) -> Expectation
testInsertPost (pool, userId) = do
    let expectedContent = Just "Hello, world!"
    let privacyLevel = PublicUnlimited
    postId <- insertPost pool userId expectedContent Nothing privacyLevel
    maybePost <- runSqlPersistMPool (get postId) pool
    let actualContent = join $ fmap postContent maybePost
    actualContent `shouldBe` expectedContent

-- Test case for retrieving visible posts
testRetrieveVisiblePosts :: (Pool SqlBackend, UserId) -> Expectation
testRetrieveVisiblePosts (pool, userId) = do
    putStrLn "Testing retrieval of visible posts..."
    setupVisibilityTestData pool userId
    posts <- runStdoutLoggingT $ getVisiblePosts pool userId
    putStrLn $ "Number of posts fetched: " ++ show (length posts)
    length posts `shouldBe` 2

-- Test case for updating a post's content
testUpdatePostContent :: (Pool SqlBackend, UserId) -> Expectation
testUpdatePostContent (pool, userId) = do
    postId <- addTestPost pool userId (Just "Old content") PublicUnlimited
    let newContent = Just "Updated content"
    updatePost pool postId newContent Nothing
    updatedPostMaybe <- runSqlPersistMPool (get postId) pool
    let actualUpdatedContent = join $ fmap postContent updatedPostMaybe
    actualUpdatedContent `shouldBe` newContent

-- Test case for deleting a post
testDeletePostById :: (Pool SqlBackend, UserId) -> Expectation
testDeletePostById (pool, userId) = do
    postId <- addTestPost pool userId (Just "To be deleted") PublicUnlimited
    deletePost pool postId
    deletedPostResult <- runSqlPersistMPool (get postId) pool
    deletedPostResult `shouldSatisfy` isNothing

-- Helper function to setup test data for visibility tests
setupVisibilityTestData :: Pool SqlBackend -> UserId -> IO ()
setupVisibilityTestData pool userId = do
    putStrLn "Setting up visibility test data..."

    -- Create a contact for the user. Assuming this adds a contact but does not return a new UserId.
    _ <- addTestContact pool userId "contact-info@example.com"

    -- Add a post with PublicLimited visibility by the main user.
    _ <- addTestPost pool userId (Just "Hello, PublicLimited!") PublicLimited

    -- If you want a post from a "contact", you should create a new user here to represent that contact
    -- and use that new UserId for posting. For simplicity, here we just use the main user.
    -- Alternatively, if contacts should post, adjust your data model or test logic accordingly.
    putStrLn "Visibility test data setup complete."