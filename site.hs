{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            compiled <- pandocCompiler
            post <- loadAndApplyTemplate "templates/post.html" postCtx compiled
            saveSnapshot "post" post
            loadAndApplyTemplate "templates/layout.html" postCtx post
                >>= relativizeUrls

    match (fromList ["about.html", "posts.html"]) $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let pageCtx = listField "posts" postCtx (return posts) `mappend` defaultContext
            getResourceBody
                >>= applyAsTemplate pageCtx
                >>= loadAndApplyTemplate "templates/layout.html" pageCtx
                >>= relativizeUrls

    match (fromList ["index.html", "posts.html"]) $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAllSnapshots "posts/*" "post"
            let indexCtx = listField "posts" postCtx (return posts) `mappend` defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/layout.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
