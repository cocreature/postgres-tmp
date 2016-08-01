{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PostgreSQL.TmpSpec (spec) where

import           Control.Concurrent (threadDelay)
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

spec :: Spec
spec =
  do describe "withTmpDB" $
       do it "should not create new roles & dbs" $
            do (roles,dbs) <- withConn defaultDB $ getRolesAndDBs
               withTmpDB $
                 \(DBInfo db role) ->
                   withConn ("dbname='" <> T.encodeUtf8 db <> "' user='" <>
                             T.encodeUtf8 role <>
                             "'")
                            (const (pure ()))
               (roles',dbs') <- withConn defaultDB $ getRolesAndDBs
               roles' `shouldBe` roles
               dbs' `shouldBe` dbs
          it "should deal with exceptions outside of conn" $
            do (roles,dbs) <- withConn defaultDB $ getRolesAndDBs
               (r :: Either DBException ()) <-
                 try $ withTmpDB $ \_ -> throwIO DBException
               r `shouldBe` Left DBException
               (roles',dbs') <- withConn defaultDB $ getRolesAndDBs
               roles' `shouldBe` roles
               dbs' `shouldBe` dbs
          it "should deal with exceptions in conn" $
            do (roles,dbs) <- withConn defaultDB $ getRolesAndDBs
               (r :: Either DBException ()) <-
                 try $
                 withTmpDB $
                 \(DBInfo db role) ->
                   withConn ("dbname='" <> T.encodeUtf8 db <> "' user='" <>
                             T.encodeUtf8 role <>
                             "'")
                            (const $ threadDelay (5*10^6) >> throwIO DBException)
               r `shouldBe` Left DBException
               (roles',dbs') <- withConn defaultDB $ getRolesAndDBs
               roles' `shouldBe` roles
               dbs' `shouldBe` dbs


data DBException = DBException deriving (Eq,Show)
instance Exception DBException