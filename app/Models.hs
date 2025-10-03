{-# LANGUAGE OverloadedStrings #-}

module Models where

import qualified Data.Text.Lazy as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- 用户数据结构
data User = User
    { userId :: Int
    , username :: T.Text
    , email :: T.Text
    , passwordHash :: T.Text
    , createdAt :: Maybe UTCTime
    } deriving (Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow (User id username email password created) = toRow (id, username, email, password, created)

-- 书架数据结构
data Bookshelf = Bookshelf
    { bookshelfId :: Int
    , bookshelfUserId :: Int
    , bookshelfNovelId :: Int
    , addedAt :: Maybe UTCTime
    } deriving (Show)

instance FromRow Bookshelf where
    fromRow = Bookshelf <$> field <*> field <*> field <*> field

instance ToRow Bookshelf where
    toRow (Bookshelf id uid nid added) = toRow (id, uid, nid, added)

-- 小说数据结构
data Novel = Novel
    { novelId :: Int
    , title :: T.Text
    , author :: T.Text
    , description :: T.Text
    , novelCreatedAt :: Maybe UTCTime
    } deriving (Show, Eq)

instance FromRow Novel where
    fromRow = Novel <$> field <*> field <*> field <*> field <*> field

instance ToRow Novel where
    toRow (Novel id title author desc created) = toRow (id, title, author, desc, created)

-- 章节数据结构
data Chapter = Chapter
    { chapterId :: Int
    , chapterNovelId :: Int
    , chapterNumber :: Int
    , chapterTitle :: T.Text
    , content :: T.Text
    , chapterCreatedAt :: Maybe UTCTime
    } deriving (Show)

instance FromRow Chapter where
    fromRow = Chapter <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Chapter where
    toRow (Chapter id nid num title content created) = toRow (id, nid, num, title, content, created)

-- 阅读进度数据结构
data ReadingProgress = ReadingProgress
    { progressId :: Int
    , progressUserId :: Int
    , progressNovelId :: Int
    , progressChapterId :: Int
    , lastReadAt :: Maybe UTCTime
    } deriving (Show)

instance FromRow ReadingProgress where
    fromRow = ReadingProgress <$> field <*> field <*> field <*> field <*> field

instance ToRow ReadingProgress where
    toRow (ReadingProgress id uid nid cid readAt) = toRow (id, uid, nid, cid, readAt)