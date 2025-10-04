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
    , searchPage
    , novelCard
    , novelCardWithProgress
    ) where

import Layout (layout, Html)
import Pages (homePage, novelsPage, novelDetailPage, chapterPage, loginPage, registerPage, bookshelfPage, aboutPage)
import Components (novelCard, novelCardWithProgress)
import Search (searchPage)