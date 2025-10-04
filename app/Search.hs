{-# LANGUAGE OverloadedStrings #-}

module Search
    ( searchPage
    ) where

import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, id)
import Models
import Components (novelCard)

-- 搜索页面
searchPage :: [Novel] -> T.Text -> H.Html
searchPage novels query = do
    H.h1 H.! class_ "page-title" $ do
        "搜索: "
        H.toHtml query
    
    H.div H.! class_ "search-results" $ do
        if null novels
            then do
                H.p H.! class_ "no-results" $ "没有找到与 '"
                H.toHtml query
                "' 相关的小说"
            else do
                H.p H.! class_ "result-count" $ do
                    "找到 "
                    H.toHtml (show (length novels))
                    " 个结果"
                
                H.div H.! class_ "novels-grid" $ do
                    mapM_ novelCard novels