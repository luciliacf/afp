--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    -- images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- CSS 
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    --- static files
    match (fromList ["contact.markdown","resources.markdown","homeworks.markdown",
                      "schedule.markdown","syllabus.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    --- lecture pages
    match "lectures/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- lectures .lhs
    match "lectures/*.lhs" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler

    -- homework pages
    match "homework/*.lhs" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- homework .hs
    match "homework/*.hs" $ do
        route   idRoute
        compile copyFileCompiler

    -- code
    match "code/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- templates
    match "templates/*" $ compile templateCompiler

    -- index
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = constField "title" "Home" `mappend` defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


--------------------------------------------------------------------------------
