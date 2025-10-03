{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as T
import Database
import Routes

main :: IO ()
main = do
    -- 初始化数据库
    initializeDatabase
    insertSampleData
    
    -- 启动Web服务器
    putStrLn "启动 FollowFlee 服务器..."
    S.scotty 3000 routes