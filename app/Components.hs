{-# LANGUAGE OverloadedStrings #-}

module Components
    ( novelCard
    , novelCardWithProgress
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Maybe (fromMaybe)
import Models
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!), toHtml)
import Text.Blaze.Html5.Attributes (href)

type Html = H.Html

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