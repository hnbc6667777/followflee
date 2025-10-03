{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import System.Directory
import System.FilePath
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.HTTP.Types (status404)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple (Only(..))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Data.Text as TS
import qualified Data.ByteString.Char8 as BSC
import Control.Exception (try, SomeException)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

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
    } deriving (Show)

instance FromRow Novel where
    fromRow = Novel <$> field <*> field <*> field <*> field <*> field

instance ToRow Novel where
    toRow (Novel id title author desc created) = toRow (id, title, author, desc, created)

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

-- 辅助函数
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

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
        result <- try $ execute conn query params
        case result of
            Right _ -> do
                lastRowId <- lastInsertRowId conn
                return $ Just (fromIntegral lastRowId)
            Left (_ :: SomeException) -> return Nothing

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
        result <- try $ execute conn query params
        case result of
            Right _ -> return True
            Left (_ :: SomeException) -> return False

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

-- 数据库操作函数
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

-- HTML模板函数
layout :: Maybe User -> H.Html -> H.Html
layout mUser content = do
    H.html $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
            H.title "小说阅读网"
            H.style ""
        H.body $ do
            H.header $ do
                H.h1 ! A.style "background: #2c3e50; color: white; padding: 1rem; margin: 0;" $ 
                    "小说阅读网"
                H.nav ! A.style "background: #34495e; padding: 0.5rem;" $ do
                    H.a ! A.href (H.toValue ("/" :: T.Text)) ! A.style "color: white; margin-right: 1rem;" $ "首页"
                    H.a ! A.href (H.toValue ("/novels" :: T.Text)) ! A.style "color: white; margin-right: 1rem;" $ "小说列表"
                    case mUser of
                        Just user -> do
                            H.a ! A.href (H.toValue ("/bookshelf" :: T.Text)) ! A.style "color: white; margin-right: 1rem;" $ "我的书架"
                            H.span ! A.style "color: white; margin-right: 1rem;" $ H.toHtml ("欢迎, " `T.append` username user)
                            H.a ! A.href (H.toValue ("/logout" :: T.Text)) ! A.style "color: white;" $ "退出"
                        Nothing -> do
                            H.a ! A.href (H.toValue ("/login" :: T.Text)) ! A.style "color: white; margin-right: 1rem;" $ "登录"
                            H.a ! A.href (H.toValue ("/register" :: T.Text)) ! A.style "color: white;" $ "注册"
                    H.a ! A.href (H.toValue ("/about" :: T.Text)) ! A.style "color: white; margin-left: 1rem;" $ "关于"
            H.main ! A.style "padding: 2rem; max-width: 1200px; margin: 0 auto;" $ content
            H.footer ! A.style "background: #ecf0f1; padding: 1rem; text-align: center; margin-top: 2rem;" $
                "© 2024 小说阅读网 - 版权所有"

-- 首页
homePage :: Maybe User -> IO H.Html
homePage mUser = do
    novels <- getAllNovels
    let recentNovels = take 3 novels
    return $ do
        H.h2 "欢迎来到小说阅读网"
        H.p "这里汇集了各种精彩的小说，免费阅读，畅享阅读乐趣！"
        
        H.div ! A.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 2rem; margin-top: 2rem;" $ do
            mapM_ (novelCard mUser) recentNovels
        
        H.div ! A.style "text-align: center; margin-top: 2rem;" $
            H.a ! A.href (H.toValue ("/novels" :: T.Text)) ! A.style "background: #3498db; color: white; padding: 0.5rem 1rem; text-decoration: none; border-radius: 4px;" $
                "查看所有小说"

novelCard :: Maybe User -> Novel -> H.Html
novelCard mUser novel = do
    H.div ! A.style "border: 1px solid #ddd; border-radius: 8px; padding: 1rem; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);" $ do
        H.h3 ! A.style "margin-top: 0;" $ H.toHtml (title novel)
        H.p ! A.style "color: #666;" $ "作者：" >> H.toHtml (author novel)
        H.p ! A.style "color: #888; font-size: 0.9rem;" $ H.toHtml (description novel)
        H.div ! A.style "display: flex; gap: 0.5rem; margin-top: 1rem;" $ do
            H.a ! A.href (H.toValue $ "novel/" `T.append` T.pack (show (novelId novel))) 
              ! A.style "background: #27ae60; color: white; padding: 0.3rem 0.8rem; text-decoration: none; border-radius: 4px; display: inline-block;" $
                "开始阅读"
            case mUser of
                Just user -> 
                    H.form ! A.method "post" ! A.action (H.toValue $ "/bookshelf/add/" `T.append` T.pack (show (novelId novel))) ! A.style "display: inline;" $ do
                        H.button ! A.type_ "submit" ! A.style "background: #e67e22; color: white; padding: 0.3rem 0.8rem; border: none; border-radius: 4px; cursor: pointer;" $
                            "添加到书架"
                Nothing -> 
                    H.a ! A.href (H.toValue ("/login" :: T.Text)) ! A.style "background: #e67e22; color: white; padding: 0.3rem 0.8rem; text-decoration: none; border-radius: 4px; display: inline-block;" $
                        "登录后添加到书架"

-- 小说列表页
novelsPage :: Maybe User -> IO H.Html
novelsPage mUser = do
    novels <- getAllNovels
    return $ do
        H.h2 "小说列表"
        H.p "所有可阅读的小说"
        
        H.div ! A.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); gap: 1.5rem; margin-top: 2rem;" $ do
            mapM_ (novelCard mUser) novels

-- 小说详情页
novelDetailPage :: Novel -> IO H.Html
novelDetailPage novel = do
    chapters <- getChaptersByNovelId (novelId novel)
    return $ do
        H.h2 $ H.toHtml (title novel)
        H.p ! A.style "color: #666;" $ "作者：" >> H.toHtml (author novel)
        H.p ! A.style "color: #888; margin-bottom: 2rem;" $ H.toHtml (description novel)
        
        H.h3 "章节列表"
        H.ul ! A.style "list-style: none; padding: 0;" $ do
            mapM_ (chapterItem (novelId novel)) chapters

chapterItem :: Int -> Chapter -> H.Html
chapterItem novelId chapter = do
    H.li ! A.style "border-bottom: 1px solid #eee; padding: 0.5rem 0;" $ do
        H.a ! A.href (H.toValue $ "/chapter/" `T.append` T.pack (show novelId) 
                `T.append` "/" `T.append` T.pack (show (chapterId chapter)))
          ! A.style "text-decoration: none; color: #3498db;" $
            H.toHtml (chapterTitle chapter)

-- 章节阅读页
chapterPage :: Novel -> Chapter -> H.Html
chapterPage novel chapter = do
    H.div ! A.style "max-width: 800px; margin: 0 auto;" $ do
        H.h2 $ H.toHtml (title novel)
        H.h3 ! A.style "color: #666; border-bottom: 2px solid #3498db; padding-bottom: 0.5rem;" $ 
            H.toHtml (chapterTitle chapter)
        
        H.div ! A.style "line-height: 1.8; font-size: 1.1rem; margin-top: 2rem;" $ do
            H.pre ! A.style "white-space: pre-wrap; font-family: inherit; background: #f8f9fa; padding: 1rem; border-radius: 4px;" $
                H.toHtml (content chapter)
        
        H.div ! A.style "margin-top: 2rem; display: flex; justify-content: space-between;" $ do
            H.a ! A.href (H.toValue $ "/novel/" `T.append` T.pack (show (novelId novel)))
              ! A.style "background: #95a5a6; color: white; padding: 0.5rem 1rem; text-decoration: none; border-radius: 4px;" $
                "返回目录"
            H.a ! A.href (H.toValue ("#" :: T.Text)) 
              ! A.style "background: #3498db; color: white; padding: 0.5rem 1rem; text-decoration: none; border-radius: 4px;" $
                "下一章"

-- 登录页面
loginPage :: H.Html
loginPage = do
    H.div ! A.style "max-width: 400px; margin: 0 auto;" $ do
        H.h2 "用户登录"
        H.form ! A.method "post" ! A.action "/login" $ do
            H.div ! A.style "margin-bottom: 1rem;" $ do
                H.label ! A.for "username" $ "用户名"
                H.br
                H.input ! A.type_ "text" ! A.id "username" ! A.name "username" ! A.required "true" ! A.style "width: 100%; padding: 0.5rem; margin-top: 0.5rem;"
            H.div ! A.style "margin-bottom: 1rem;" $ do
                H.label ! A.for "password" $ "密码"
                H.br
                H.input ! A.type_ "password" ! A.id "password" ! A.name "password" ! A.required "true" ! A.style "width: 100%; padding: 0.5rem; margin-top: 0.5rem;"
            H.button ! A.type_ "submit" ! A.style "background: #3498db; color: white; padding: 0.5rem 1rem; border: none; border-radius: 4px; width: 100%;" $
                "登录"
        H.p ! A.style "text-align: center; margin-top: 1rem;" $
            H.a ! A.href (H.toValue ("/register" :: T.Text)) $ "还没有账号？立即注册"

-- 注册页面
registerPage :: H.Html
registerPage = do
    H.div ! A.style "max-width: 400px; margin: 0 auto;" $ do
        H.h2 "用户注册"
        H.form ! A.method "post" ! A.action "/register" $ do
            H.div ! A.style "margin-bottom: 1rem;" $ do
                H.label ! A.for "username" $ "用户名"
                H.br
                H.input ! A.type_ "text" ! A.id "username" ! A.name "username" ! A.required "true" ! A.style "width: 100%; padding: 0.5rem; margin-top: 0.5rem;"
            H.div ! A.style "margin-bottom: 1rem;" $ do
                H.label ! A.for "email" $ "邮箱"
                H.br
                H.input ! A.type_ "email" ! A.id "email" ! A.name "email" ! A.required "true" ! A.style "width: 100%; padding: 0.5rem; margin-top: 0.5rem;"
            H.div ! A.style "margin-bottom: 1rem;" $ do
                H.label ! A.for "password" $ "密码"
                H.br
                H.input ! A.type_ "password" ! A.id "password" ! A.name "password" ! A.required "true" ! A.style "width: 100%; padding: 0.5rem; margin-top: 0.5rem;"
            H.button ! A.type_ "submit" ! A.style "background: #27ae60; color: white; padding: 0.5rem 1rem; border: none; border-radius: 4px; width: 100%;" $
                "注册"
        H.p ! A.style "text-align: center; margin-top: 1rem;" $
            H.a ! A.href (H.toValue ("/login" :: T.Text)) $ "已有账号？立即登录"

-- 书架页面
bookshelfPage :: User -> IO H.Html
bookshelfPage user = do
    novels <- getUserBookshelf (userId user)
    return $ do
        H.h2 $ "我的书架 - " >> H.toHtml (username user)
        if null novels
            then H.p "您的书架还是空的，快去添加一些喜欢的小说吧！"
            else H.div ! A.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); gap: 1.5rem; margin-top: 2rem;" $ do
                mapM_ (bookshelfNovelCard user) novels

bookshelfNovelCard :: User -> Novel -> H.Html
bookshelfNovelCard user novel = do
    H.div ! A.style "border: 1px solid #ddd; border-radius: 8px; padding: 1rem; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);" $ do
        H.h3 ! A.style "margin-top: 0;" $ H.toHtml (title novel)
        H.p ! A.style "color: #666;" $ "作者：" >> H.toHtml (author novel)
        H.p ! A.style "color: #888; font-size: 0.9rem;" $ H.toHtml (description novel)
        H.div ! A.style "display: flex; gap: 0.5rem; margin-top: 1rem;" $ do
            H.a ! A.href (H.toValue $ "novel/" `T.append` T.pack (show (novelId novel))) 
              ! A.style "background: #27ae60; color: white; padding: 0.3rem 0.8rem; text-decoration: none; border-radius: 4px; display: inline-block;" $
                "开始阅读"
            H.form ! A.method "post" ! A.action (H.toValue $ "/bookshelf/remove/" `T.append` T.pack (show (novelId novel))) ! A.style "display: inline;" $ do
                H.button ! A.type_ "submit" ! A.style "background: #e74c3c; color: white; padding: 0.3rem 0.8rem; border: none; border-radius: 4px; cursor: pointer;" $
                    "从书架移除"

-- 关于页面
aboutPage :: H.Html
aboutPage = do
    H.h2 "关于我们"
    H.p "小说阅读网致力于为读者提供优质的在线阅读体验。"
    H.p "我们收集整理各类精彩小说，让您随时随地享受阅读的乐趣。"
    
    H.h3 "特色功能"
    H.ul $ do
        H.li "海量小说资源，持续更新"
        H.li "简洁美观的阅读界面"
        H.li "支持多种设备访问"
        H.li "完全免费阅读"
        H.li "个人书架功能，收藏喜欢的小说"
        H.li "用户登录注册，个性化体验"

-- 简单的会话管理（在实际项目中应该使用更安全的方案）
type Session = Maybe User

-- 全局变量存储当前登录用户（简化版，实际应该使用更安全的会话管理）
currentUser :: IORef (Maybe User)
currentUser = unsafePerformIO (newIORef Nothing)

-- 从请求中获取用户（简化版，使用内存存储）
getUserFromRequest :: S.ActionM (Maybe User)
getUserFromRequest = do
    liftIO $ readIORef currentUser

-- 主程序
main :: IO ()
main = do
    -- 初始化数据库
    initializeDatabase
    insertSampleData
    
    S.scotty 3000 $ do
        -- 首页
        S.get "/" $ do
            mUser <- getUserFromRequest
            page <- liftIO $ homePage mUser
            S.html $ R.renderHtml $ layout mUser page
        
        -- 小说列表页
        S.get "/novels" $ do
            mUser <- getUserFromRequest
            page <- liftIO $ novelsPage mUser
            S.html $ R.renderHtml $ layout mUser page
        
        -- 小说详情页
        S.get "/novel/:id" $ do
            mUser <- getUserFromRequest
            novelId <- S.param "id"
            novel <- liftIO $ getNovelById novelId
            case novel of
                Just n -> do
                    page <- liftIO $ novelDetailPage n
                    S.html $ R.renderHtml $ layout mUser page
                Nothing -> do
                    S.status status404
                    S.text "小说未找到"
        
        -- 章节阅读页
        S.get "/chapter/:novelId/:chapterId" $ do
            mUser <- getUserFromRequest
            nid <- S.param "novelId"
            cid <- S.param "chapterId"
            novel <- liftIO $ getNovelById nid
            case novel of
                Just n -> do
                    chapter <- liftIO $ getChapterById nid cid
                    case chapter of
                        Just c -> S.html $ R.renderHtml $ layout mUser $ chapterPage n c
                        Nothing -> do
                            S.status status404
                            S.text "章节未找到"
                Nothing -> do
                    S.status status404
                    S.text "小说未找到"
        
        -- 登录页面
        S.get "/login" $ do
            mUser <- getUserFromRequest
            case mUser of
                Just _ -> S.redirect "/"
                Nothing -> S.html $ R.renderHtml $ layout Nothing loginPage
        
        -- 登录处理
        S.post "/login" $ do
            username <- S.formParam "username"
            password <- S.formParam "password"
            mUser <- liftIO $ authenticateUser (TS.pack username) (TS.pack password)
            case mUser of
                Just user -> do
                    -- 设置当前登录用户
                    liftIO $ writeIORef currentUser (Just user)
                    S.redirect "/"
                Nothing -> do
                    S.html $ R.renderHtml $ layout Nothing $ do
                        H.div ! A.style "max-width: 400px; margin: 0 auto;" $ do
                            H.h2 "登录失败"
                            H.p "用户名或密码错误"
                            H.a ! A.href (H.toValue ("/login" :: T.Text)) $ "返回登录"
        
        -- 注册页面
        S.get "/register" $ do
            mUser <- getUserFromRequest
            case mUser of
                Just _ -> S.redirect "/"
                Nothing -> S.html $ R.renderHtml $ layout Nothing registerPage
        
        -- 注册处理
        S.post "/register" $ do
            username <- S.formParam "username"
            email <- S.formParam "email"
            password <- S.formParam "password"
            mUserId <- liftIO $ createUser (TS.pack username) (TS.pack email) (TS.pack password)
            case mUserId of
                Just _ -> S.redirect "/login"
                Nothing -> do
                    S.html $ R.renderHtml $ layout Nothing $ do
                        H.div ! A.style "max-width: 400px; margin: 0 auto;" $ do
                            H.h2 "注册失败"
                            H.p "用户名或邮箱已存在"
                            H.a ! A.href (H.toValue ("/register" :: T.Text)) $ "返回注册"
        
        -- 书架页面
        S.get "/bookshelf" $ do
            mUser <- getUserFromRequest
            case mUser of
                Just user -> do
                    page <- liftIO $ bookshelfPage user
                    S.html $ R.renderHtml $ layout mUser page
                Nothing -> S.redirect "/login"
        
        -- 添加到书架
        S.post "/bookshelf/add/:novelId" $ do
            mUser <- getUserFromRequest
            case mUser of
                Just user -> do
                    novelId <- S.param "novelId"
                    success <- liftIO $ addToBookshelf (userId user) novelId
                    if success
                        then S.redirect "/bookshelf"
                        else S.text "添加失败，可能已经存在于书架中"
                Nothing -> S.redirect "/login"
        
        -- 从书架移除
        S.post "/bookshelf/remove/:novelId" $ do
            mUser <- getUserFromRequest
            case mUser of
                Just user -> do
                    novelId <- S.param "novelId"
                    success <- liftIO $ removeFromBookshelf (userId user) novelId
                    if success
                        then S.redirect "/bookshelf"
                        else S.text "移除失败"
                Nothing -> S.redirect "/login"
        
        -- 退出登录
        S.get "/logout" $ do
            -- 清除当前登录用户
            liftIO $ writeIORef currentUser Nothing
            S.redirect "/"
        
        -- 关于页面
        S.get "/about" $ do
            mUser <- getUserFromRequest
            S.html $ R.renderHtml $ layout mUser aboutPage
        
        -- 静态文件服务（可选）
        S.get "/static/:file" $ do
            file <- S.param "file"
            S.file $ "static/" ++ file

-- 辅助函数（已迁移到数据库操作）
