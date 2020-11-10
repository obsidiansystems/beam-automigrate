{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module SubDatabases where

import Data.Text (Text)
import Data.Proxy
import Data.Typeable

import GHC.Generics
import Control.Exception

import Database.Beam.Postgres
import Database.Beam.Schema ( Beamable
                            , Columnar
                            , Database
                            , DatabaseSettings
                            , PrimaryKey
                            , TableEntity
                            , dbModification
                            , defaultDbSettings
                            , fieldNamed
                            , setEntityName
                            , modifyTableFields
                            , tableModification
                            , withDbModification
                            )
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables (primaryKey)
import Database.Beam.Query (currentTimestamp_)

import Database.Beam.AutoMigrate.Annotated

import Database.Beam.AutoMigrate ( Schema
                                 , Diff
                                 , Migration
                                 , fromAnnotatedDbSettings
                                 , defaultAnnotatedDbSettings
                                 , diff
                                 , unsafeRunMigration
                                 , printMigration
                                 , migrate
                                 )
import Database.Beam.AutoMigrate.Postgres (getSchema)

import qualified Database.PostgreSQL.Simple as Pg

import Data.Int (Int32 , Int64)
import Data.Time (LocalTime)

--
-- Example showcasing sub-databases and foreign key discovery.
--

type RoomKind = (* -> *) -> *
type UserKind = (* -> *) -> *

-- Sub database type
data ChatRoomT (room :: RoomKind) (user :: UserKind) (f :: * -> *) = Chat
  { _chat_messages :: f (TableEntity (MessageT room user))
  , _chat_likes    :: f (TableEntity (LikeT room user))
  } deriving Generic

deriving instance
  ( Typeable room
  , Typeable user
  , Beamable user
  , Beamable room
  , Beamable (PrimaryKey room)
  , Beamable (PrimaryKey user)
  ) => Database be (ChatRoomT room user)

-- Table
data MessageT (room :: RoomKind) (user :: UserKind) (f :: * -> *) = Message
  { _message_id      :: Columnar f Int64
  -- ^ Primary key (or we could have (room, sentAt) or something like that);
  -- this is exposed from Chat somehow as an available foreign key target
  , _message_room    :: PrimaryKey room f
  , _message_sentAt  :: Columnar f LocalTime
  , _message_sender  :: PrimaryKey user f
  , _message_body    :: Columnar f Text
  } deriving (Generic)

deriving instance ( Beamable (PrimaryKey room)
                  , Beamable room
                  , Beamable user
                  , Beamable (PrimaryKey user)
                  ) => Beamable (MessageT room user)

instance ( Typeable room
         , Typeable user
         , Beamable user
         , Beamable room
         , Beamable (PrimaryKey room)
         , Beamable (PrimaryKey user)
         )
  => Beam.Table (MessageT room user) where
  data PrimaryKey (MessageT room user) f =
    MessageID (Columnar f Int64) (PrimaryKey room f)
    deriving (Generic)
  primaryKey = MessageID . _message_id <*> _message_room

deriving instance (Beamable (PrimaryKey room), Beamable room) => Beamable (PrimaryKey (MessageT room user))

-- Table describing the \"likes\" a particular user (identified by a PK) leaves on a particular message of
-- a particular chatroom.
data LikeT (room :: RoomKind) (user :: UserKind) (f :: * -> *) = Like
  { _like_message :: PrimaryKey (MessageT room user) f
  -- ^ Internal foreign key
  , _like_user    :: PrimaryKey user f
  -- ^ External foreign key
  } deriving (Generic)

deriving instance ( Beamable room
                  , Beamable (PrimaryKey room)
                  , Beamable (PrimaryKey user)
                  )
  => Beamable (LikeT room user)

instance ( Typeable user
         , Typeable room
         , Beamable room
         , Beamable (PrimaryKey user)
         , Beamable (PrimaryKey room)
         )
  => Beam.Table (LikeT room user) where
  data PrimaryKey (LikeT room user) f =
    LikeID (PrimaryKey (MessageT room user) f) (PrimaryKey user f)
    deriving (Generic)
  primaryKey = LikeID . _like_message <*> _like_user

deriving instance ( Beamable (PrimaryKey user)
                  , Beamable (PrimaryKey room)
                  , Beamable room
                  )
  => Beamable (PrimaryKey (LikeT room user))

data UserT f = User
  { _userNickname :: Columnar f Text
  , _userRealName :: Columnar f Text
  } deriving (Generic, Beamable)

instance Beam.Table UserT where
  data PrimaryKey UserT f =
    UserID (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = UserID . _userNickname

newtype ComplianceOfficerT f = ComplianceOfficer
  { _complianceOfficer :: UserT f
  } deriving (Generic, Beamable)

instance Beam.Table ComplianceOfficerT where
  data PrimaryKey ComplianceOfficerT f =
    OfficerID (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = OfficerID . _userNickname . _complianceOfficer

data FlowerAuditRoom (f :: * -> *) = FlowerAuditRoom
  { _flower_audit_room_id :: Columnar f Int32
  } deriving (Generic, Beamable)

instance Beam.Table FlowerAuditRoom where
  data PrimaryKey FlowerAuditRoom f =
    FlowerAuditRoomID (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = FlowerAuditRoomID . _flower_audit_room_id

data UserRoom (f :: * -> *)   = UserRoom
  { _user_room_id :: Columnar f Int32
  } deriving (Generic, Beamable)

instance Beam.Table UserRoom where
  data PrimaryKey UserRoom f =
    UserRoomID (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserRoomID . _user_room_id

data FlowerRoom (f :: * -> *) = FlowerRoom
  { _flower_room_id :: Columnar f Int32
  } deriving (Generic, Beamable)

instance Beam.Table FlowerRoom where
  data PrimaryKey FlowerRoom f =
    FlowerRoomID (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = FlowerRoomID . _flower_room_id

--
-- The main database
--

data AppDB (f :: * -> *) = AppDB
  {  _appDb_users            :: f (TableEntity UserT)
  -- ^ All the standard users registered in our chat program.

  -- , _appDb_users2            :: f (TableEntity UserT)
  -- ^ The above is commented out as an example to create ambiguity.

  , _appDb_complianceOfficers :: f (TableEntity ComplianceOfficerT)
  -- ^ Users who are responsible for regulatory compliance and recordkeeping; separate from regular users
  -- for Very Important Reasons (TM)
  --
  -- Chat rooms begin here.
  --
  , _appDb_flowerChannel      :: f (TableEntity FlowerRoom)
  , _appDb_flowerChatRoom     :: (ChatRoomT FlowerRoom UserT f)
  -- ^ Discussion of each type of flower, published by standard users.
  , _appDb_userChannel        :: f (TableEntity UserRoom)
  , _appDb_userChatRoom       :: (ChatRoomT UserRoom UserT f)
  -- ^ The gossip zone, general discussion on users, published by standard users.
  , _appDb_complianceChannel  :: f (TableEntity FlowerAuditRoom)
  , _appDb_complianceChatRoom :: (ChatRoomT FlowerAuditRoom ComplianceOfficerT f)
  -- ^ Discussion of legal obligations regarding particular messages about flowers
  } deriving (Generic)

deriving instance
  ( Beamable (PrimaryKey FlowerRoom)
  , Beamable (PrimaryKey UserRoom)
  , Beamable (PrimaryKey FlowerAuditRoom)
  ) => Database be AppDB

-- FlowerAuditsRoom is a type that somehow specifies that its foreign key should be pointing at the
-- messages inside of _appDb_flowerChat

appDB :: DatabaseSettings Postgres AppDB
appDB = defaultDbSettings  `withDbModification` dbModification
  { _appDb_users = setEntityName "users"

  , _appDb_complianceOfficers = setEntityName "auditors"

  , _appDb_flowerChatRoom     = (dbModification @_ @_ @(ChatRoomT FlowerRoom UserT))
    { _chat_messages = setEntityName "flowers_chat_messages"
                    <> modifyTableFields tableModification { _message_sentAt = fieldNamed "sent_at" }
    , _chat_likes    = setEntityName "flowers_chat_likes"
    }

  , _appDb_userChatRoom = (dbModification @_ @_ @(ChatRoomT UserRoom UserT))
    { _chat_messages = setEntityName "users_chat_messages"
                    <> modifyTableFields tableModification { _message_sentAt = fieldNamed "sent_at" }
    , _chat_likes    = setEntityName "users_chat_likes"
    }

  , _appDb_complianceChatRoom = (dbModification @_ @_ @(ChatRoomT FlowerAuditRoom ComplianceOfficerT))
    { _chat_messages = setEntityName "auditors_chat_messages"
                    <> modifyTableFields tableModification { _message_sentAt = fieldNamed "sent_at" }
    , _chat_likes    = setEntityName "auditors_chat_likes"
    }

 }

annotatedDB :: AnnotatedDatabaseSettings Postgres AppDB
annotatedDB = defaultAnnotatedDbSettings appDB `withDbModification` dbModification
  { _appDb_flowerChatRoom = dbModification {
        _chat_messages = annotateTableFields tableModification  {
          _message_sentAt = defaultsTo currentTimestamp_
        }
    }
  }

hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings annotatedDB manualFks
  where
    -- Uncomment this if you want to check what happens if you introduce a 'user2' table.
    -- manualFks = Proxy @'[ 'UserDefinedFk (MessageT UserRoom UserT)
    --                     , 'UserDefinedFk (MessageT FlowerRoom UserT)
    --                     , 'UserDefinedFk (LikeT UserRoom UserT)
    --                     , 'UserDefinedFk (LikeT FlowerRoom UserT)
    --                     ]
    manualFks = Proxy @'[]

getDbSchema :: String -> IO Schema
getDbSchema dbName =
  bracket (connect defaultConnectInfo { connectUser = "adinapoli", connectDatabase = dbName }) close getSchema

getFlowerDbSchema :: IO Schema
getFlowerDbSchema = getDbSchema "beam-test-subdb"

getSchemaDiff :: IO Diff
getSchemaDiff = diff hsSchema <$> getFlowerDbSchema

exampleShowMigration :: IO ()
exampleShowMigration = withBeamTestDb printMigration

withBeamTestDb :: (Migration Pg -> Pg ()) -> IO ()
withBeamTestDb action = do
  let connInfo = "host=localhost port=5432 dbname=beam-test-subdb"
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ do
      let mig = migrate conn hsSchema
      action mig

exampleAutoMigration :: IO ()
exampleAutoMigration = withBeamTestDb unsafeRunMigration
