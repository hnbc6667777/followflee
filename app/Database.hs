{-# LANGUAGE OverloadedStrings #-}

module Database
    ( initializeDatabase
    , insertSampleData
    , createUser
    , getUserByUsername
    , getUserById
    , authenticateUser
    , addToBookshelf
    , removeFromBookshelf
    , getUserBookshelf
    , isInBookshelf
    , saveReadingProgress
    , getReadingProgress
    , getAllReadingProgress
    , getAllNovels
    , getNovelById
    , getChaptersByNovelId
    , getChapterById
    ) where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Exception (try, SomeException)
import Database.SQLite.Simple
import Database.SQLite.Simple (Only(..))
import Models
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)

-- 数据库文件路径
databasePath :: FilePath
databasePath = "followflee.db"

-- 初始化数据库
initializeDatabase :: IO ()
initializeDatabase = do
    conn <- open databasePath
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT UNIQUE NOT NULL, email TEXT UNIQUE NOT NULL, password_hash TEXT NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
    execute_ conn "CREATE TABLE IF NOT EXISTS novels (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, author TEXT NOT NULL, description TEXT, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
    execute_ conn "CREATE TABLE IF NOT EXISTS chapters (id INTEGER PRIMARY KEY AUTOINCREMENT, novel_id INTEGER NOT NULL, chapter_number INTEGER NOT NULL, title TEXT NOT NULL, content TEXT, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY (novel_id) REFERENCES novels (id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS bookshelf (id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER NOT NULL, novel_id INTEGER NOT NULL, added_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY (user_id) REFERENCES users (id), FOREIGN KEY (novel_id) REFERENCES novels (id), UNIQUE(user_id, novel_id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS reading_progress (id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER NOT NULL, novel_id INTEGER NOT NULL, chapter_id INTEGER NOT NULL, last_read_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY (user_id) REFERENCES users (id), FOREIGN KEY (novel_id) REFERENCES novels (id), FOREIGN KEY (chapter_id) REFERENCES chapters (id), UNIQUE(user_id, novel_id))"
    close conn
    putStrLn "数据库初始化完成"

-- 插入示例数据（如果数据库为空）
insertSampleData :: IO ()
insertSampleData = do
    conn <- open databasePath
    
    -- 检查是否已有数据
    novelCount <- query_ conn "SELECT COUNT(*) FROM novels" :: IO [Only Int]
    case novelCount of
        [Only 0] -> do
            -- 插入示例小说
            execute conn "INSERT INTO novels (title, author, description) VALUES (?,?,?)" 
                ("剑来" :: T.Text, "烽火戏诸侯" :: T.Text, "一个关于剑与江湖的故事" :: T.Text)
            execute conn "INSERT INTO novels (title, author, description) VALUES (?,?,?)" 
                ("凡人修仙传" :: T.Text, "忘语" :: T.Text, "一个凡人修仙的传奇故事" :: T.Text)
            execute conn "INSERT INTO novels (title, author, description) VALUES (?,?,?)" 
                ("斗破苍穹" :: T.Text, "天蚕土豆" :: T.Text, "三十年河东，三十年河西，莫欺少年穷！" :: T.Text)
            
            -- 插入示例章节
            execute conn "INSERT INTO chapters (novel_id, chapter_number, title, content) VALUES (?,?,?,?)" 
                (1 :: Int, 1 :: Int, "第一章 少年" :: T.Text, "少年从山村走出，踏上修仙之路..." :: T.Text)
            execute conn "INSERT INTO chapters (novel_id, chapter_number, title, content) VALUES (?,?,?,?)" 
                (1 :: Int, 2 :: Int, "第二章 奇遇" :: T.Text, "少年在山中遇到神秘老人..." :: T.Text)
            execute conn "INSERT INTO chapters (novel_id, chapter_number, title, content) VALUES (?,?,?,?)" 
                (2 :: Int, 1 :: Int, "第一章 山村少年" :: T.Text, "韩立出生在一个小山村..." :: T.Text)
            execute conn "INSERT INTO chapters (novel_id, chapter_number, title, content) VALUES (?,?,?,?)" 
                (2 :: Int, 2 :: Int, "第二章 入门测试" :: T.Text, "韩立参加修仙门派测试..." :: T.Text)
            execute conn "INSERT INTO chapters (novel_id, chapter_number, title, content) VALUES (?,?,?,?)" 
                (3 :: Int, 1 :: Int, "第一章 废物" :: T.Text, "萧炎从天才沦为废物..." :: T.Text)
            execute conn "INSERT INTO chapters (novel_id, chapter_number, title, content) VALUES (?,?,?,?)" 
                (3 :: Int, 2 :: Int, "第二章 药老" :: T.Text, "萧炎遇到神秘灵魂体药老..." :: T.Text)
            
            putStrLn "示例数据插入完成"
        _ -> putStrLn "数据库中已有数据，跳过示例数据插入"
    
    close conn

-- 密码哈希函数
hashPassword :: TS.Text -> IO (Maybe TS.Text)
hashPassword password = do
    let passwordBS = BSC.pack (TS.unpack password)
    mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy passwordBS
    case mHash of
        Just hash -> return $ Just (TS.pack (BSC.unpack hash))
        Nothing -> return Nothing

validatePasswordHash :: TS.Text -> TS.Text -> Bool
validatePasswordHash password hash = 
    let passwordBS = BSC.pack (TS.unpack password)
        hashBS = BSC.pack (TS.unpack hash)
    in validatePassword hashBS passwordBS

-- 用户相关数据库操作
-- 创建用户
createUser :: TS.Text -> TS.Text -> TS.Text -> IO (Maybe Int)
createUser username email password = do
    mHashedPassword <- hashPassword password
    case mHashedPassword of
        Just hashedPassword -> do
            conn <- open databasePath
            result <- tryExecute conn "INSERT INTO users (username, email, password_hash) VALUES (?,?,?)" (username, email, hashedPassword)
            close conn
            return result
        Nothing -> return Nothing
  where
    tryExecute conn query params = do
        result <- try (execute conn query params :: IO ()) :: IO (Either SomeException ())
        case result of
            Right _ -> do
                lastRowId <- lastInsertRowId conn
                return $ Just (fromIntegral lastRowId)
            Left _ -> return Nothing

-- 根据用户名获取用户
getUserByUsername :: TS.Text -> IO (Maybe User)
getUserByUsername username = do
    conn <- open databasePath
    users <- query conn "SELECT id, username, email, password_hash, created_at FROM users WHERE username = ?" (Only username)
    close conn
    case users of
        [user] -> return (Just user)
        _ -> return Nothing

-- 根据用户ID获取用户
getUserById :: Int -> IO (Maybe User)
getUserById uid = do
    conn <- open databasePath
    users <- query conn "SELECT id, username, email, password_hash, created_at FROM users WHERE id = ?" (Only uid)
    close conn
    case users of
        [user] -> return (Just user)
        _ -> return Nothing

-- 验证用户登录
authenticateUser :: TS.Text -> TS.Text -> IO (Maybe User)
authenticateUser username password = do
    mUser <- getUserByUsername username
    case mUser of
        Just user -> 
            if validatePasswordHash password (TS.pack (T.unpack (passwordHash user)))
                then return (Just user)
                else return Nothing
        Nothing -> return Nothing

-- 书架相关数据库操作
-- 添加小说到书架
addToBookshelf :: Int -> Int -> IO Bool
addToBookshelf userId novelId = do
    conn <- open databasePath
    result <- tryExecute conn "INSERT INTO bookshelf (user_id, novel_id) VALUES (?,?)" (userId, novelId)
    close conn
    return result
  where
    tryExecute conn query params = do
        result <- try (execute conn query params) :: IO (Either SomeException ())
        case result of
            Right _ -> return True
            Left _ -> return False

-- 从书架移除小说
removeFromBookshelf :: Int -> Int -> IO Bool
removeFromBookshelf userId novelId = do
    conn <- open databasePath
    execute conn "DELETE FROM bookshelf WHERE user_id = ? AND novel_id = ?" (userId, novelId)
    close conn
    return True

-- 获取用户的书架
getUserBookshelf :: Int -> IO [Novel]
getUserBookshelf userId = do
    conn <- open databasePath
    novels <- query conn "SELECT n.id, n.title, n.author, n.description, n.created_at FROM novels n JOIN bookshelf b ON n.id = b.novel_id WHERE b.user_id = ? ORDER BY b.added_at DESC" (Only userId)
    close conn
    return novels

-- 检查小说是否在用户书架中
isInBookshelf :: Int -> Int -> IO Bool
isInBookshelf userId novelId = do
    conn <- open databasePath
    items <- query conn "SELECT COUNT(*) FROM bookshelf WHERE user_id = ? AND novel_id = ?" (userId, novelId) :: IO [Only Int]
    close conn
    case items of
        [Only count] -> return (count > 0)
        _ -> return False

-- 阅读进度相关数据库操作
-- 保存阅读进度
saveReadingProgress :: Int -> Int -> Int -> IO Bool
saveReadingProgress userId novelId chapterId = do
    conn <- open databasePath
    result <- tryExecute conn "INSERT OR REPLACE INTO reading_progress (user_id, novel_id, chapter_id) VALUES (?,?,?)" (userId, novelId, chapterId)
    close conn
    return result
  where
    tryExecute conn query params = do
        result <- try (execute conn query params) :: IO (Either SomeException ())
        case result of
            Right _ -> return True
            Left _ -> return False

-- 获取用户的阅读进度
getReadingProgress :: Int -> Int -> IO (Maybe ReadingProgress)
getReadingProgress userId novelId = do
    conn <- open databasePath
    progress <- query conn "SELECT id, user_id, novel_id, chapter_id, last_read_at FROM reading_progress WHERE user_id = ? AND novel_id = ?" (userId, novelId)
    close conn
    case progress of
        [p] -> return (Just p)
        _ -> return Nothing

-- 获取用户所有小说的阅读进度
getAllReadingProgress :: Int -> IO [ReadingProgress]
getAllReadingProgress userId = do
    conn <- open databasePath
    progress <- query conn "SELECT id, user_id, novel_id, chapter_id, last_read_at FROM reading_progress WHERE user_id = ? ORDER BY last_read_at DESC" (Only userId)
    close conn
    return progress

-- 小说相关数据库操作
-- 获取所有小说
getAllNovels :: IO [Novel]
getAllNovels = do
    conn <- open databasePath
    novels <- query_ conn "SELECT id, title, author, description, created_at FROM novels ORDER BY created_at DESC"
    close conn
    return novels

-- 根据ID获取小说
getNovelById :: Int -> IO (Maybe Novel)
getNovelById novelId = do
    conn <- open databasePath
    novels <- query conn "SELECT id, title, author, description, created_at FROM novels WHERE id = ?" (Only novelId)
    close conn
    case novels of
        [novel] -> return (Just novel)
        _ -> return Nothing

-- 获取小说的所有章节
getChaptersByNovelId :: Int -> IO [Chapter]
getChaptersByNovelId novelId = do
    conn <- open databasePath
    chapters <- query conn "SELECT id, novel_id, chapter_number, title, content, created_at FROM chapters WHERE novel_id = ? ORDER BY chapter_number" (Only novelId)
    close conn
    return chapters

-- 根据小说ID和章节ID获取章节
getChapterById :: Int -> Int -> IO (Maybe Chapter)
getChapterById novelId chapterId = do
    conn <- open databasePath
    chapters <- query conn "SELECT id, novel_id, chapter_number, title, content, created_at FROM chapters WHERE novel_id = ? AND id = ?" (novelId, chapterId)
    close conn
    case chapters of
        [chapter] -> return (Just chapter)
        _ -> return Nothing