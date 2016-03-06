
-- module Nightwatch.DBInternal where
-- import Database.Persist
-- import Database.Persist.Sqlite
-- import Database.Persist.TH
-- import Nightwatch.Types
-- import qualified Data.Text as T

-- -- TODO: What's the best way to reduce this code duplication?

-- instance PersistField URL where
--   toPersistValue (URL url) = PersistText (T.pack url)
--   fromPersistValue (PersistText url) = Right (URL (T.unpack url))
--   fromPersistValue x = Left $ T.pack $ "Error in de-serializing URL from the database. Found " ++ (show x)

-- instance PersistFieldSql URL where
--   sqlType _ = SqlString

-- instance PersistField Aria2Gid where
--   toPersistValue (Aria2Gid gid) = PersistText (T.pack gid)
--   fromPersistValue (PersistText gid) = Right (Aria2Gid (T.unpack gid))
--   fromPersistValue x = Left $ T.pack $ "Error in de-serializing Aria2GID from the database. Found " ++ (show x)

-- instance PersistFieldSql Aria2Gid where
--   sqlType _ = SqlString

-- instance PersistField TgramUserId where
--   toPersistValue (TgramUserId x) = PersistInt64 (fromIntegral x)
--   fromPersistValue (PersistInt64 x) = Right (TgramUserId (fromIntegral x))
--   fromPersistValue x = Left $ T.pack $ "Error in de-serializing TgramUserId from the database. Found " ++ (show x)

-- instance PersistFieldSql TgramUserId where
--   sqlType _ = SqlInt64

-- instance PersistField TgramChatId where
--   toPersistValue (TgramChatId x) = PersistInt64 (fromIntegral x)
--   fromPersistValue (PersistInt64 x) = Right (TgramChatId (fromIntegral x))
--   fromPersistValue x = Left $ T.pack $ "Error in de-serializing TgramChatId from the database. Found " ++ (show x)

-- instance PersistFieldSql TgramChatId where
--   sqlType _ = SqlInt64


-- derivePersistField "TgramUsername"
-- derivePersistField "TgramFirstName"
-- derivePersistField "TgramLastName"
-- derivePersistField "TgramMsgText"
-- derivePersistField "Aria2RequestId"
