{-# LANGUAGE OverloadedStrings #-}

module Pages
    ( homePage
    , novelsPage
    , novelDetailPage
    , chapterPage
    , loginPage
    , registerPage
    , bookshelfPage
    , aboutPage
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Maybe (fromMaybe)
import Models
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!), toHtml)
import Text.Blaze.Html5.Attributes (href)
import Components (novelCard, novelCardWithProgress)

type Html = H.Html

-- 首页
homePage :: Html
homePage = do
    H.h1 "欢迎来到 FollowFlee"
    H.p "这是一个简单的小说阅读平台，您可以在这里阅读各种小说，管理个人书架。"
    H.div ! A.style "margin-top: 2rem;" $ do
        H.a ! A.href "/novels" ! A.class_ "btn" $ "浏览小说"
        H.a ! A.href "/bookshelf" ! A.class_ "btn btn-secondary" ! A.style "margin-left: 1rem;" $ "我的书架"
    H.div ! A.style "margin-top: 1rem;" $ do
        H.a ! A.href "/login" ! A.class_ "btn" ! A.style "margin-right: 0.5rem;" $ "登录"
        H.a ! A.href "/register" ! A.class_ "btn btn-success" $ "注册"

-- 小说列表页面
novelsPage :: [Novel] -> Html
novelsPage novels = do
    H.h1 "小说列表"
    H.div ! A.class_ "novel-grid" $ do
        mapM_ novelCard novels

-- 小说详情页面
novelDetailPage :: Novel -> [Chapter] -> Bool -> Html
novelDetailPage novel chapters isInBookshelf = do
    H.h1 $ toHtml (title novel)
    H.p $ do
        H.strong "作者："
        toHtml (author novel)
    H.p $ do
        H.strong "简介："
        toHtml (description novel)
    H.div ! A.style "margin: 2rem 0;" $ do
        if isInBookshelf
            then H.a ! A.href (H.toValue ("/bookshelf/remove/" <> T.pack (show (novelId novel)))) ! A.class_ "btn btn-secondary" $ "从书架移除"
            else H.a ! A.href (H.toValue ("/bookshelf/add/" <> T.pack (show (novelId novel)))) ! A.class_ "btn btn-success" $ "加入书架"
    H.h2 "章节列表"
    H.ul ! A.style "list-style: none; padding: 0;" $ do
        mapM_ (\chapter -> 
            H.li ! A.style "margin-bottom: 0.5rem;" $ 
                H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)) <> "/chapter/" <> T.pack (show (chapterId chapter)))) $ 
                    toHtml (chapterTitle chapter)
            ) chapters

-- 章节阅读页面
chapterPage :: Novel -> Chapter -> Maybe Chapter -> Maybe Chapter -> Html
chapterPage novel chapter prevChapter nextChapter = do
    H.div ! A.class_ "font-controls" ! A.id "font-controls-panel" $ do
        H.span "字体大小："
        H.button ! A.onclick "changeFontSize('small')" $ "小"
        H.button ! A.onclick "changeFontSize('medium')" $ "中"
        H.button ! A.onclick "changeFontSize('large')" $ "大"
        H.button ! A.onclick "changeFontSize('xlarge')" $ "特大"
    H.div ! A.class_ "text-color-controls" ! A.id "text-color-controls-panel" $ do
        H.p ! A.style "margin-bottom: 10px; font-weight: bold;" $ "文字颜色："
        H.div ! A.style "display: flex; gap: 10px; margin-bottom: 10px;" $ do
            H.button ! A.onclick "changeTextColor('black')" ! A.style "background-color: #000000; color: white; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "黑色"
            H.button ! A.onclick "changeTextColor('dark-gray')" ! A.style "background-color: #333333; color: white; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "深灰"
            H.button ! A.onclick "changeTextColor('gray')" ! A.style "background-color: #666666; color: white; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "灰色"
            H.button ! A.onclick "changeTextColor('brown')" ! A.style "background-color: #8B4513; color: white; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "棕色"
            H.button ! A.onclick "changeTextColor('dark-blue')" ! A.style "background-color: #000080; color: white; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "深蓝"
        H.div ! A.style "display: flex; align-items: center; gap: 10px;" $ do
            H.span "自定义："
            H.input ! A.type_ "color" ! A.onchange "changeCustomTextColor(this.value)" ! A.style "width: 50px; height: 30px;"
    H.div ! A.class_ "background-controls" ! A.id "background-controls-panel" $ do
        H.p ! A.style "margin-bottom: 10px; font-weight: bold;" $ "背景颜色："
        H.div ! A.style "display: flex; gap: 10px; margin-bottom: 10px;" $ do
            H.button ! A.onclick "changeBackgroundColor('white')" ! A.style "background-color: white; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "白色"
            H.button ! A.onclick "changeBackgroundColor('#f5f5dc')" ! A.style "background-color: #f5f5dc; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "米色"
            H.button ! A.onclick "changeBackgroundColor('#e8f4f8')" ! A.style "background-color: #e8f4f8; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "浅蓝"
            H.button ! A.onclick "changeBackgroundColor('#f0f8f0')" ! A.style "background-color: #f0f8f0; border: 1px solid #ccc; padding: 5px 10px; cursor: pointer;" $ "浅绿"
        H.div ! A.style "display: flex; align-items: center; gap: 10px;" $ do
            H.span "自定义："
            H.input ! A.type_ "color" ! A.onchange "changeCustomBackgroundColor(this.value)" ! A.style "width: 50px; height: 30px;"

    H.button ! A.id "toggle-font-controls" ! A.class_ "btn btn-secondary" ! A.style "position: fixed; top: 60px; right: 20px; z-index: 1001;" ! A.onclick "toggleFontControls()" $ "字体设置"
    H.button ! A.id "toggle-text-color-controls" ! A.class_ "btn btn-secondary" ! A.style "position: fixed; top: 100px; right: 20px; z-index: 1001;" ! A.onclick "toggleTextColorControls()" $ "文字颜色"
    H.button ! A.id "toggle-background-controls" ! A.class_ "btn btn-secondary" ! A.style "position: fixed; top: 140px; right: 20px; z-index: 1001;" ! A.onclick "toggleBackgroundControls()" $ "背景设置"
    H.h1 $ toHtml (chapterTitle chapter)
    H.p $ do
        H.strong "小说："
        toHtml (title novel)
    H.div ! A.id "chapter-content" ! A.class_ "chapter-content" $ toHtml (content chapter)
    H.div ! A.class_ "chapter-nav" $ do
        case prevChapter of
            Just prev -> H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)) <> "/chapter/" <> T.pack (show (chapterId prev)))) ! A.class_ "btn" $ "上一章"
            Nothing -> H.span ""
        H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)))) ! A.class_ "btn btn-secondary" $ "返回目录"
        case nextChapter of
            Just next -> H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)) <> "/chapter/" <> T.pack (show (chapterId next)))) ! A.class_ "btn" $ "下一章"
            Nothing -> H.span ""

-- 登录页面
loginPage :: Maybe T.Text -> Html
loginPage errorMsg = do
    H.h1 "登录"
    case errorMsg of
        Just msg -> H.div ! A.class_ "alert alert-error" $ toHtml msg
        Nothing -> ""
    H.form ! A.method "post" ! A.action "/login" ! A.enctype "application/x-www-form-urlencoded" $ do
        H.div ! A.class_ "form-group" $ do
            H.label ! A.class_ "form-label" $ "用户名："
            H.input ! A.type_ "text" ! A.name "username" ! A.class_ "form-input" ! A.required ""
        H.div ! A.class_ "form-group" $ do
            H.label ! A.class_ "form-label" $ "密码："
            H.input ! A.type_ "password" ! A.name "password" ! A.class_ "form-input" ! A.required ""
        H.button ! A.type_ "submit" ! A.class_ "btn" $ "登录"
    H.p ! A.style "margin-top: 1rem;" $ do
        "还没有账号？"
        H.a ! A.href "/register" $ "立即注册"

-- 注册页面
registerPage :: Maybe T.Text -> Html
registerPage errorMsg = do
    H.h1 "注册"
    case errorMsg of
        Just msg -> H.div ! A.class_ "alert alert-error" $ toHtml msg
        Nothing -> ""
    H.form ! A.method "post" ! A.action "/register" ! A.enctype "application/x-www-form-urlencoded" $ do
        H.div ! A.class_ "form-group" $ do
            H.label ! A.class_ "form-label" $ "用户名："
            H.input ! A.type_ "text" ! A.name "username" ! A.class_ "form-input" ! A.required ""
        H.div ! A.class_ "form-group" $ do
            H.label ! A.class_ "form-label" $ "邮箱："
            H.input ! A.type_ "email" ! A.name "email" ! A.class_ "form-input" ! A.required ""
        H.div ! A.class_ "form-group" $ do
            H.label ! A.class_ "form-label" $ "密码："
            H.input ! A.type_ "password" ! A.name "password" ! A.class_ "form-input" ! A.required ""
        H.button ! A.type_ "submit" ! A.class_ "btn btn-success" $ "注册"
    H.p ! A.style "margin-top: 1rem;" $ do
        "已有账号？"
        H.a ! A.href "/login" $ "立即登录"

-- 书架页面
bookshelfPage :: [Novel] -> [(Novel, Maybe ReadingProgress)] -> Html
bookshelfPage novels progress = do
    H.h1 "我的书架"
    if null novels
        then H.p $ do
            "您的书架还没有小说，"
            H.a ! A.href "/novels" $ "去添加一些吧"
        else H.div ! A.class_ "novel-grid" $ do
            mapM_ (\novel -> 
                let mProgress = case lookup novel progress of
                                 Just progressValue -> progressValue
                                 Nothing -> Nothing
                in novelCardWithProgress novel mProgress) novels

-- 关于页面
aboutPage :: Html
aboutPage = do
    H.h1 "关于 FollowFlee"
    H.p "FollowFlee 是一个简单的小说阅读平台，提供以下功能："
    H.ul ! A.style "margin-left: 2rem; margin-top: 1rem;" $ do
        H.li "浏览和阅读各种小说"
        H.li "管理个人书架"
        H.li "记录阅读进度"
        H.li "自定义字体大小"
    H.p ! A.style "margin-top: 2rem;" $ "感谢使用 FollowFlee！"