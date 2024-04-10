{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server_Code.Server where

{-
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString.Char8 qualified as BS
import Data.Pool (Pool)
import Database.Persist.Postgresql (
  SqlBackend,
  createPostgresqlPool,
 )
import Network.Wai (Request)
import Network.Wai.Handler.Warp (run)
import Relude.Lifted.Env qualified as Env
import Servant (type (:<|>) (..), Handler, hoistServer, HasServer (ServerT))
import Servant.Server
    ( serveWithContext, Server, Context(..), Application )
import Servant.Server.Experimental.Auth ( AuthHandler )
import Server_Code.API (
  DocumentAPI,
 )
 -}
{-
import Server_Code.Types ( AuthenticatedUser, AppM, AppEnv (AppEnv) )
import Servant.Multipart
import Servant.Auth.Server
import Data_Storage.Model
-}
{-
-- Combine all API parts
type API = DocumentAPI

myContext :: Context '[AuthHandler Request AuthenticatedUser]
myContext = authHandler :. EmptyContext

-- Main server function that composes all parts
server :: AppEnv -> Server API
server env = documentServer env

convertApp :: AppEnv -> ReaderT AppEnv Handler a -> Handler a
convertApp env r = runReaderT r env

--     :<|> userServer pool
--     :<|> contactServer pool
--    :<|> trustedContactServer pool
--    :<|> newsfeedServer pool

-- Dedicated server function for DocumentAPI
-- Assuming getAllDocumentsHandler is adjusted to work with AppEnv
documentServer :: ServerT DocumentAPI (ReaderT AppEnv Handler)
documentServer =
  getAllDocumentsHandler
    :<|> addDocumentHandler

-- Matches "POST /documents"
 -- :<|> getAllDocumentsHandler pool -- Matches "GET /documents"
--  :<|> getDocumentHandler pool-- Matches "GET /documents/:documentId"
--  :<|> updateDocumentHandler pool -- Matches "PUT /documents/:documentId"
--  :<|> deleteDocumentHandler pool -- Matches "DELETE /documents/:documentId"

-- contactServer :: Pool SqlBackend -> Server ContactAPI
-- contactServer pool =
--    getAllContactsHandler pool
--       :<|> addContactHandler pool
--     :<|> getContactHandler pool
--       :<|> updateContactHandler pool -- Assuming you've implemented this
--       :<|> deleteContactHandler pool

-- Dedicated server function for UserAPI
-- userServer :: Pool SqlBackend -> Server UserAPI
-- userServer pool =
--    registerUserHandler pool
--       :<|> handleUserLogin pool
--       :<|> getUserByIdHandler pool
--       :<|> updateUserHandler pool
--        :<|> passwordRecoveryHandler pool

-- Main application entry point
app :: AppEnv -> Application
app env = serveWithContext apiProxy myContext (server env)

apiProxy :: Proxy API
apiProxy = Proxy

main :: IO ()
main = do
  connStr <- Env.lookupEnv "DATABASE_URL"
  let defaultConnStr = BS.pack "host=localhost dbname=mydb user=postgres password=postgres port=5432"
  let connStrBS = maybe defaultConnStr BS.pack connStr
  pool <- runStderrLoggingT $ createPostgresqlPool connStrBS 10
  let appEnv = AppEnv pool -- Initialize your AppEnv with the created pool
  run 8080 (app appEnv) -- Start the application with the configured environment
  -}