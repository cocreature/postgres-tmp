{-| Create temporary postgresql databases.

The main usecase for this are tests where you don’t want to assume that a certain database exists.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PostgreSQL.Tmp 
  (withTmpDB
  ,withTmpDB'
  ,newRole
  ,newDB
  ,defaultDB
  ,DBInfo(..)) where

import           Control.Applicative (pure)
import           Control.Exception
import           Data.ByteString (ByteString)
import           Data.Coerce
import           Data.Int
import           Data.Monoid
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

-- | Connection string for the @postgres@ database owned by the
-- @postgres@ user
defaultDB :: ByteString
defaultDB = "dbname='postgres' user='postgres'"

-- | The data necessary to connect to the temporary database
data DBInfo =
  DBInfo {dbName :: T.Text
         ,roleName :: T.Text} deriving (Show,Read,Eq,Ord)

-- | Convenience wrapper for 'withTmpDB'' using 'defaultDB'
withTmpDB :: (DBInfo -> IO a) -> IO a
withTmpDB = withTmpDB' defaultDB

-- | Create a temporary database and a temporary role that the
-- callback can operate on. After the action has finished the database
-- and the role are destroyed.
--
-- This function assumes that the connection string points to a
-- database containing the tables called @pg_roles@ and @pg_database@
-- and that the user has the @CREATEDB@ and @CREATEROLE@ privileges.
withTmpDB' :: ByteString -> (DBInfo -> IO a) -> IO a
withTmpDB' conStr f =
  bracket (connectPostgreSQL conStr) close $
    \conn ->
       bracket (newRole conn) (dropRole conn) $ \role -> do
       bracket (newDB conn role) (dropDatabase conn) $ \db -> do
         f (DBInfo {dbName = db, roleName = role})

-- | Create a new role that does not already exist and return its name.
--
-- The new role does not have a password and has the @CREATEDB@
-- privilege. The database that the connection points to is assumed to
-- contain a table called @pg_roles@ with a @rolname@ column.
newRole :: Connection -> IO T.Text
newRole conn =
  do (roles :: [Only T.Text]) <- query_ conn "SELECT rolname FROM pg_roles"
     let newName = freshName "tmp" (coerce roles)
     _ <- execute conn "CREATE USER ? WITH CREATEDB" (Only (Identifier newName))
     pure newName

-- | Drop the role.
dropRole :: Connection -> T.Text -> IO Int64
dropRole conn name = execute conn "DROP ROLE ?" (Only (Identifier name))

-- | Create a new database that is owned by the user.
newDB :: Connection -> T.Text -> IO T.Text
newDB conn role =
  do (dbNames :: [Only T.Text]) <- query_ conn "SELECT datname FROM pg_database"
     let newName = freshName "tmp" (coerce dbNames)
     _ <- execute conn "CREATE DATABASE ? OWNER ?" (Identifier newName,Identifier role)
     pure newName

-- | Drop the database.
dropDatabase :: Connection -> T.Text -> IO Int64
dropDatabase conn name =
  execute conn "DROP DATABASE ?" (Only (Identifier name))

-- | Create a fresh name that is not in the list of already existing names.
--
-- The fresh name is generated by appending a number to the supplied
-- template.
freshName :: T.Text -> [T.Text] -> T.Text
freshName template existingNames = loop 0
-- We could use a Set here to speed up the lookup, however the
-- construction of that Set is linear as well so it would only pay off
-- if at least one of the lookups fails.
  where loop :: Int -> T.Text
        loop i =
          if (template <> T.pack (show i)) `elem` existingNames
             then loop (i + 1)
             else (template <> T.pack (show i))
