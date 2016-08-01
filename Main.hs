{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (AsyncException(..))
import           Control.Concurrent
import           Control.Exception.Safe
import           Data.ByteString (ByteString)
import           Data.Coerce
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Tmp
import           Test.Hspec

withConn :: ByteString -> (Connection -> IO a) -> IO a
withConn connStr = bracket (connectPostgreSQL connStr) close

getRolesAndDBs :: Connection -> IO ([T.Text],[T.Text])
getRolesAndDBs conn =
  do (roles :: [Only T.Text]) <- query_ conn "SELECT rolname from pg_roles"
     (dbs :: [Only T.Text]) <- query_ conn "SELECT datname from pg_database"
     pure (coerce roles,coerce dbs)

main :: IO ()
main = 
  do
  _ <- forkIO $ do (roles,dbs) <- withConn defaultDB $ getRolesAndDBs
                   (r :: Either AsyncException ()) <-
                     try $
                     withTmpDB $
                     \(DBInfo db role) ->
                       withConn ("dbname='" <> T.encodeUtf8 db <> "' user='" <>
                                 T.encodeUtf8 role <>
                                 "'")
                                (const $ threadDelay (5*10^6) >> throwIO UserInterrupt)
                   print $ r == Left UserInterrupt
                   (roles',dbs') <- withConn defaultDB $ getRolesAndDBs
                   print $ roles' == roles
                   print $ dbs' == dbs
  threadDelay (5*10^6)
