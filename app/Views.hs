{-# LANGUAGE OverloadedStrings #-}

module Views
    ( layout
    , homePage
    , novelsPage
    , novelDetailPage
    , chapterPage
    , loginPage
    , registerPage
    , bookshelfPage
    , aboutPage
    , novelCard
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Maybe (fromMaybe)
import Models
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), toHtml, docTypeHtml)
import Text.Blaze.Html5.Attributes (href)

type Html = H.Html

-- 布局模板
layout :: T.Text -> Html -> T.Text
layout title content = renderHtml $ docTypeHtml $ do
    H.html ! A.lang "zh-CN" $ do
        H.head $ do
            H.meta ! A.charset "UTF-8"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
            H.title $ toHtml (title <> " - FollowFlee")
            H.style $ toHtml cssStyles
        H.body $ do
            H.div ! A.class_ "header" $ do
                H.div ! A.class_ "container" $ do
                    H.h1 "FollowFlee - 小说阅读平台"
            H.div ! A.class_ "nav" $ do
                H.div ! A.class_ "container" $ do
                    H.a ! A.href "/" $ "首页"
                    H.a ! A.href "/novels" $ "小说列表"
                    H.a ! A.href "/bookshelf" $ "我的书架"
                    H.a ! A.href "/about" $ "关于"
            H.div ! A.class_ "main" $ do
                H.div ! A.class_ "container" $ content
            H.div ! A.class_ "footer" $ do
                H.div ! A.class_ "container" $ do
                    H.p "© 2024 FollowFlee. All rights reserved."
            H.script $ toHtml javascriptCode

cssStyles :: T.Text
cssStyles = T.unlines
    [ "* { margin: 0; padding: 0; box-sizing: border-box; }"
    , "body { font-family: 'Microsoft YaHei', sans-serif; line-height: 1.6; color: #333; background-color: #f5f5f5; }"
    , ".container { max-width: 1200px; margin: 0 auto; padding: 0 20px; }"
    , ".header { background: #2c3e50; color: white; padding: 1rem 0; }"
    , ".header h1 { font-size: 2rem; margin-bottom: 0.5rem; }"
    , ".nav { background: #34495e; padding: 0.5rem 0; }"
    , ".nav a { color: white; text-decoration: none; margin-right: 1rem; padding: 0.5rem 1rem; border-radius: 3px; transition: background 0.3s; }"
    , ".nav a:hover { background: #2c3e50; }"
    , ".main { padding: 2rem 0; }"
    , ".footer { background: #34495e; color: white; text-align: center; padding: 1rem 0; margin-top: 2rem; }"
    , ".novel-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 2rem; margin-top: 2rem; }"
    , ".novel-card { background: white; border-radius: 8px; padding: 1.5rem; box-shadow: 0 2px 10px rgba(0,0,0,0.1); transition: transform 0.3s; }"
    , ".novel-card:hover { transform: translateY(-5px); }"
    , ".novel-title { font-size: 1.5rem; color: #2c3e50; margin-bottom: 0.5rem; }"
    , ".novel-author { color: #7f8c8d; margin-bottom: 1rem; }"
    , ".novel-description { color: #555; line-height: 1.6; }"
    , ".btn { display: inline-block; background: #3498db; color: white; padding: 0.5rem 1rem; text-decoration: none; border-radius: 4px; border: none; cursor: pointer; transition: background 0.3s; }"
    , ".btn:hover { background: #2980b9; }"
    , ".btn-secondary { background: #95a5a6; }"
    , ".btn-secondary:hover { background: #7f8c8d; }"
    , ".btn-success { background: #27ae60; }"
    , ".btn-success:hover { background: #229954; }"
    , ".form-group { margin-bottom: 1rem; }"
    , ".form-label { display: block; margin-bottom: 0.5rem; font-weight: bold; }"
    , ".form-input { width: 100%; padding: 0.5rem; border: 1px solid #ddd; border-radius: 4px; font-size: 1rem; }"
    , ".chapter-content { background: white; padding: 2rem; border-radius: 8px; line-height: 1.8; font-size: 1.1rem; }"
    , ".chapter-nav { display: flex; justify-content: space-between; margin-top: 2rem; }"
    , ".font-controls { position: fixed; top: 100px; right: 20px; background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); z-index: 1000; display: block; }"
    , ".font-controls button { margin: 0 2px; padding: 5px 10px; border: 1px solid #ddd; background: #f8f9fa; cursor: pointer; border-radius: 3px; }"
    , ".font-controls button.active { background: #3498db; color: white; border-color: #3498db; }"
    , ".background-controls { position: fixed; top: 160px; right: 20px; background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); z-index: 1000; display: block; }"
    , ".background-controls button { margin: 0 2px; padding: 5px 10px; border: 1px solid #ddd; cursor: pointer; border-radius: 3px; }"
    , ".background-controls button.active { border: 2px solid #3498db; }"
    , ".font-small { font-size: 14px; }"
    , ".font-medium { font-size: 16px; }"
    , ".font-large { font-size: 18px; }"
    , ".font-xlarge { font-size: 20px; }"
    , ".alert { padding: 1rem; border-radius: 4px; margin-bottom: 1rem; }"
    , ".alert-success { background: #d4edda; color: #155724; border: 1px solid #c3e6cb; }"
    , ".alert-error { background: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }"
    ]

javascriptCode :: T.Text
javascriptCode = T.unlines
    [ "function changeFontSize(size) {"
    , "    const content = document.getElementById('chapter-content');"
    , "    if (content) {"
    , "        content.className = 'chapter-content font-' + size;"
    , "        localStorage.setItem('fontSize', size);"
    , "        // 更新按钮激活状态"
    , "        document.querySelectorAll('.font-controls button').forEach(btn => {"
    , "            btn.classList.remove('active');"
    , "        });"
    , "        event.target.classList.add('active');"
    , "    }"
    , "}"
    , ""
    , "function toggleFontControls() {"
    , "    const panel = document.getElementById('font-controls-panel');"
    , "    const toggleBtn = document.getElementById('toggle-font-controls');"
    , "    if (panel && toggleBtn) {"
    , "        if (panel.style.display === 'none') {"
    , "            panel.style.display = 'block';"
    , "            toggleBtn.textContent = '隐藏字体设置';"
    , "            localStorage.setItem('fontControlsVisible', 'true');"
    , "        } else {"
    , "            panel.style.display = 'none';"
    , "            toggleBtn.textContent = '字体设置';"
    , "            localStorage.setItem('fontControlsVisible', 'false');"
    , "        }"
    , "    }"
    , "}"
    , ""
    , "function changeBackgroundColor(color) {"
    , "    const content = document.getElementById('chapter-content');"
    , "    if (content) {"
    , "        content.style.backgroundColor = color;"
    , "        localStorage.setItem('backgroundColor', color);"
    , "        // 更新按钮激活状态"
    , "        document.querySelectorAll('.background-controls button').forEach(btn => {"
    , "            btn.classList.remove('active');"
    , "        });"
    , "        event.target.classList.add('active');"
    , "    }"
    , "}"
    , ""
    , "function changeCustomBackgroundColor(color) {"
    , "    const content = document.getElementById('chapter-content');"
    , "    if (content) {"
    , "        content.style.backgroundColor = color;"
    , "        localStorage.setItem('backgroundColor', color);"
    , "        // 移除所有按钮的激活状态"
    , "        document.querySelectorAll('.background-controls button').forEach(btn => {"
    , "            btn.classList.remove('active');"
    , "        });"
    , "    }"
    , "}"
    , ""
    , "function toggleBackgroundControls() {"
    , "    const panel = document.getElementById('background-controls-panel');"
    , "    const toggleBtn = document.getElementById('toggle-background-controls');"
    , "    if (panel && toggleBtn) {"
    , "        if (panel.style.display === 'none') {"
    , "            panel.style.display = 'block';"
    , "            toggleBtn.textContent = '隐藏背景设置';"
    , "            localStorage.setItem('backgroundControlsVisible', 'true');"
    , "        } else {"
    , "            panel.style.display = 'none';"
    , "            toggleBtn.textContent = '背景设置';"
    , "            localStorage.setItem('backgroundControlsVisible', 'false');"
    , "        }"
    , "    }"
    , "}"
    , ""
    , "function initFontSize() {"
    , "    const content = document.getElementById('chapter-content');"
    , "    if (content) {"
    , "        const savedSize = localStorage.getItem('fontSize') || 'medium';"
    , "        content.className = 'chapter-content font-' + savedSize;"
    , "        // 设置按钮激活状态"
    , "        document.querySelectorAll('.font-controls button').forEach(btn => {"
    , "            if (btn.textContent.includes(savedSize.charAt(0).toUpperCase() + savedSize.slice(1))) {"
    , "                btn.classList.add('active');"
    , "            }"
    , "        });"
    , "        "
    , "        // 初始化背景颜色"
    , "        const savedColor = localStorage.getItem('backgroundColor') || 'white';"
    , "        content.style.backgroundColor = savedColor;"
    , "        // 设置背景按钮激活状态"
    , "        document.querySelectorAll('.background-controls button').forEach(btn => {"
    , "            if (btn.textContent === '白色' && savedColor === 'white') {"
    , "                btn.classList.add('active');"
    , "            } else if (btn.textContent === '米色' && savedColor === '#f5f5dc') {"
    , "                btn.classList.add('active');"
    , "            } else if (btn.textContent === '浅蓝' && savedColor === '#e8f4f8') {"
    , "                btn.classList.add('active');"
    , "            } else if (btn.textContent === '浅绿' && savedColor === '#f0f8f0') {"
    , "                btn.classList.add('active');"
    , "            }"
    , "        });"
    , "    }"
    , "    // 初始化字体控制面板显示状态"
    , "    const fontPanel = document.getElementById('font-controls-panel');"
    , "    const fontToggleBtn = document.getElementById('toggle-font-controls');"
    , "    if (fontPanel && fontToggleBtn) {"
    , "        const isFontVisible = localStorage.getItem('fontControlsVisible') !== 'false';"
    , "        if (!isFontVisible) {"
    , "            fontPanel.style.display = 'none';"
    , "            fontToggleBtn.textContent = '字体设置';"
    , "        } else {"
    , "            fontToggleBtn.textContent = '隐藏字体设置';"
    , "        }"
    , "    }"
    , "    "
    , "    // 初始化背景控制面板显示状态"
    , "    const bgPanel = document.getElementById('background-controls-panel');"
    , "    const bgToggleBtn = document.getElementById('toggle-background-controls');"
    , "    if (bgPanel && bgToggleBtn) {"
    , "        const isBgVisible = localStorage.getItem('backgroundControlsVisible') !== 'false';"
    , "        if (!isBgVisible) {"
    , "            bgPanel.style.display = 'none';"
    , "            bgToggleBtn.textContent = '背景设置';"
    , "        } else {"
    , "            bgToggleBtn.textContent = '隐藏背景设置';"
    , "        }"
    , "    }"
    , "}"
    , ""
    , "document.addEventListener('DOMContentLoaded', initFontSize);"
    ]

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
    H.button ! A.id "toggle-background-controls" ! A.class_ "btn btn-secondary" ! A.style "position: fixed; top: 100px; right: 20px; z-index: 1001;" ! A.onclick "toggleBackgroundControls()" $ "背景设置"
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

-- 小说卡片组件
novelCard :: Novel -> Html
novelCard novel = do
    H.div ! A.class_ "novel-card" $ do
        H.h3 ! A.class_ "novel-title" $ toHtml (title novel)
        H.p ! A.class_ "novel-author" $ do
            "作者："
            toHtml (author novel)
        H.p ! A.class_ "novel-description" $ toHtml (description novel)
        H.div ! A.style "margin-top: 1rem;" $ do
            H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)))) ! A.class_ "btn" $ "查看详情"

-- 带阅读进度的小说卡片
novelCardWithProgress :: Novel -> Maybe ReadingProgress -> Html
novelCardWithProgress novel mProgress = do
    H.div ! A.class_ "novel-card" $ do
        H.h3 ! A.class_ "novel-title" $ toHtml (title novel)
        H.p ! A.class_ "novel-author" $ do
            "作者："
            toHtml (author novel)
        H.p ! A.class_ "novel-description" $ toHtml (description novel)
        case mProgress of
            Just progress -> 
                H.p ! A.style "color: #27ae60; margin-top: 0.5rem;" $ do
                    "阅读进度：第"
                    toHtml (T.pack (show (progressChapterId progress)))
                    "章"
            Nothing -> ""
        H.div ! A.style "margin-top: 1rem;" $ do
            case mProgress of
                Just progress -> 
                    H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)) <> "/chapter/" <> T.pack (show (progressChapterId progress)))) ! A.class_ "btn" $ "继续阅读"
                Nothing -> 
                    H.a ! A.href (H.toValue ("/novel/" <> T.pack (show (novelId novel)))) ! A.class_ "btn" $ "开始阅读"
            H.a ! A.href (H.toValue ("/bookshelf/remove/" <> T.pack (show (novelId novel)))) ! A.class_ "btn btn-secondary" ! A.style "margin-left: 0.5rem;" $ "移除"