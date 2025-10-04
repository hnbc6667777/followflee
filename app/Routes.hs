{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( routes
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as TS
import qualified Web.Scotty as S
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, toString, fromString)
import Data.UUID.V4 (nextRandom)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import System.IO.Unsafe (unsafePerformIO)
import Models
import Database
import Views
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- 会话管理类型
data SessionToken = SessionToken { sessionUserId :: Int, sessionCreatedAt :: T.Text }

-- 会话存储（使用Map实现）
type SessionStore = M.Map T.Text SessionToken

-- 全局会话存储（简化实现，生产环境应使用数据库或Redis）
{-# NOINLINE sessionStore #-}
sessionStore :: MVar SessionStore
sessionStore = unsafePerformIO $ newMVar M.empty

-- 获取当前用户ID
getCurrentUserId :: S.ActionM (Maybe Int)
getCurrentUserId = do
    -- 从Cookie中获取会话ID
    mSessionId <- S.header "Cookie"
    case mSessionId of
        Just cookieHeader -> do
            let sessionId = extractSessionIdFromCookie cookieHeader
            case sessionId of
                Just sid -> do
                    -- 从会话存储中获取用户ID
                    store <- liftIO $ readMVar sessionStore
                    case M.lookup sid store of
                        Just token -> return $ Just (sessionUserId token)
                        Nothing -> return Nothing
                Nothing -> return Nothing
        Nothing -> return Nothing

-- 创建新会话
createSession :: Int -> S.ActionM T.Text
createSession uid = do
    sessionId <- liftIO $ toString <$> nextRandom
    currentTime <- liftIO $ T.pack . show <$> getCurrentTime
    let token = SessionToken { sessionUserId = uid, sessionCreatedAt = currentTime }
    liftIO $ modifyMVar_ sessionStore $ \store ->
        return $ M.insert (T.pack sessionId) token store
    return (T.pack sessionId)

-- 从Cookie中提取会话ID
extractSessionIdFromCookie :: T.Text -> Maybe T.Text
extractSessionIdFromCookie cookieHeader =
    case find (T.isPrefixOf "session_id=") (T.split (== ';') cookieHeader) of
        Just cookiePart ->
            case T.split (== '=') cookiePart of
                [key, value] | key == "session_id" -> Just value
                _ -> Nothing
        Nothing -> Nothing

-- 路由定义
routes :: S.ScottyM ()
routes = do
    -- 首页
    S.get "/" $ do
        S.html $ layout "首页" homePage
    
    -- 小说列表
    S.get "/novels" $ do
        novels <- liftIO getAllNovels
        S.html $ layout "小说列表" (novelsPage novels)
    
    -- 小说详情
    S.get "/novel/:novelId" $ do
        novelId <- S.param "novelId"
        mNovel <- liftIO $ getNovelById novelId
        case mNovel of
            Just novel -> do
                chapters <- liftIO $ getChaptersByNovelId novelId
                mUserId <- getCurrentUserId
                case mUserId of
                    Just userId -> do
                        isInShelf <- liftIO $ isInBookshelf userId novelId
                        S.html $ layout (title novel) (novelDetailPage novel chapters isInShelf)
                    Nothing -> do
                        S.html $ layout (title novel) (novelDetailPage novel chapters False)
            Nothing -> do
                S.text "小说不存在"
    
    -- 章节阅读
    S.get "/novel/:novelId/chapter/:chapterId" $ do
        novelIdParam <- S.param "novelId"
        chapterIdParam <- S.param "chapterId"
        mNovel <- liftIO $ getNovelById novelIdParam
        mChapter <- liftIO $ getChapterById novelIdParam chapterIdParam
        case (mNovel, mChapter) of
            (Just novel, Just chapter) -> do
                -- 获取前后章节
                chapters <- liftIO $ getChaptersByNovelId novelIdParam
                let chapterIndex = findIndex (\c -> chapterId c == chapterIdParam) chapters
                let prevChapter = case chapterIndex of
                        Just idx | idx > 0 -> Just (chapters !! (idx - 1))
                        _ -> Nothing
                let nextChapter = case chapterIndex of
                        Just idx | idx < length chapters - 1 -> Just (chapters !! (idx + 1))
                        _ -> Nothing
                
                -- 保存阅读进度
                mUserId <- getCurrentUserId
                case mUserId of
                    Just userId -> liftIO $ saveReadingProgress userId novelIdParam chapterIdParam
                    Nothing -> liftIO $ return True
                
                S.html $ layout (chapterTitle chapter) (chapterPage novel chapter prevChapter nextChapter)
            _ -> do
                S.text "章节不存在"
    
    -- 登录页面
    S.get "/login" $ do
        S.html $ layout "登录" (loginPage Nothing)
    
    -- 登录处理
    S.post "/login" $ do
        username <- S.formParam "username"
        password <- S.formParam "password"
        mUser <- liftIO $ authenticateUser (TS.pack (T.unpack username)) (TS.pack (T.unpack password))
        case mUser of
            Just user -> do
                -- 创建会话并设置Cookie
                sessionId <- createSession (userId user)
                S.setHeader "Set-Cookie" ("session_id=" <> sessionId <> "; Path=/; HttpOnly")
                S.redirect "/"
            Nothing -> do
                S.html $ layout "登录" (loginPage (Just "用户名或密码错误"))
    
    -- 注册页面
    S.get "/register" $ do
        S.html $ layout "注册" (registerPage Nothing)
    
    -- 注册处理
    S.post "/register" $ do
        username <- S.formParam "username"
        email <- S.formParam "email"
        password <- S.formParam "password"
        mUserId <- liftIO $ createUser (TS.pack (T.unpack username)) (TS.pack (T.unpack email)) (TS.pack (T.unpack password))
        case mUserId of
            Just uid -> do
                -- 创建会话并设置Cookie
                sessionId <- createSession uid
                S.setHeader "Set-Cookie" ("session_id=" <> sessionId <> "; Path=/; HttpOnly")
                S.redirect "/"
            Nothing -> do
                S.html $ layout "注册" (registerPage (Just "注册失败，用户名或邮箱可能已存在"))
    
    -- 书架页面
    S.get "/bookshelf" $ do
        mUserId <- getCurrentUserId
        case mUserId of
            Just userId -> do
                novels <- liftIO $ getUserBookshelf userId
                progress <- liftIO $ getAllReadingProgress userId
                let novelProgress = map (\novel -> (novel, findProgressForNovel novel progress)) novels
                S.html $ layout "我的书架" (bookshelfPage novels novelProgress)
            Nothing -> do
                S.redirect "/login"
    
    -- 添加到书架
    S.get "/bookshelf/add/:novelId" $ do
        novelId <- S.param "novelId"
        mUserId <- getCurrentUserId
        case mUserId of
            Just userId -> do
                success <- liftIO $ addToBookshelf userId novelId
                if success
                    then S.redirect "/bookshelf"
                    else S.text "添加失败"
            Nothing -> do
                S.redirect "/login"
    
    -- 从书架移除
    S.get "/bookshelf/remove/:novelId" $ do
        novelId <- S.param "novelId"
        mUserId <- getCurrentUserId
        case mUserId of
            Just userId -> do
                success <- liftIO $ removeFromBookshelf userId novelId
                if success
                    then S.redirect "/bookshelf"
                    else S.text "移除失败"
            Nothing -> do
                S.redirect "/login"
    
    -- 登出
    S.get "/logout" $ do
        -- 清除会话Cookie
        S.setHeader "Set-Cookie" "session_id=; Path=/; HttpOnly; Expires=Thu, 01 Jan 1970 00:00:00 GMT"
        S.redirect "/"
    
    -- 关于页面
    S.get "/about" $ do
        S.html $ layout "关于" aboutPage
    
    -- 搜索页面
    S.get "/search" $ do
        query <- S.param "q"
        novels <- liftIO $ searchNovels query
        S.html $ layout ("搜索: " <> query) (searchPage novels query)

-- 辅助函数：从阅读进度获取小说ID
getNovelFromProgress :: ReadingProgress -> Novel
getNovelFromProgress progress = 
    Novel (progressNovelId progress) "" "" "" Nothing

-- 辅助函数：查找小说对应的阅读进度
findProgressForNovel :: Novel -> [ReadingProgress] -> Maybe ReadingProgress
findProgressForNovel novel progressList = 
    case filter (\p -> progressNovelId p == novelId novel) progressList of
        [] -> Nothing
        (p:_) -> Just p

-- 辅助函数：查找元素索引
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex pred xs = go 0 xs
  where
    go _ [] = Nothing
    go n (x:xs)
        | pred x = Just n
        | otherwise = go (n + 1) xs