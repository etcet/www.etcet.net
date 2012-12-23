{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Control.Monad (forM_)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc (WriterOptions(..), defaultWriterOptions)

import Hakyll

main :: IO ()
main = hakyllWith config $ do
  
    -- Compress CSS
    match "stylesheets/*" $ do
      route   idRoute
      compile compressCssCompiler
    
    -- Copy fonts
    match "fonts/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy images
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy post images
    match "images/posts/*" $ do
      route   idRoute
      compile copyFileCompiler
    
    -- Copy javascripts
    match "javascripts/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy files
    match "files/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy project files
    match "projects/*" $ do
      route   idRoute
      compile copyFileCompiler
    match "projects/*/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy robots.txt
    match "robots.txt" $ do
      route   idRoute
      compile copyFileCompiler
          
    -- Render all pages (blog posts and projects)
    match "posts/*/*" $ do
      route   $ setExtension ".html"
      compile $ wunkiCompiler
        >>> arr (renderDateField "date" "%Y-%m-%d" "Date unknown")
        >>> arr (setField "bodyclass" "post")
        >>> renderTagsField "prettytags" (fromCapture "tags/*")
        >>> applyTemplateCompiler "templates/page.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Post list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All Posts")
        >>> arr (setField "bodyclass" "postlist")
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/blog/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- project list
    match "projects.html" $ route idRoute
    create "projects.html" $ constA mempty
        >>> arr (setField "title" "All Projects")
        >>> arr (setField "bodyclass" "postlist")
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/projects/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- all pages list
    match "pages.html" $ route idRoute
    create "pages.html" $ constA mempty
        >>> arr (setField "title" "All Posts &amp; Projects")
        >>> arr (setField "bodyclass" "postlist")
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/*/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "etcet.net")
        >>> arr (setField "description" description)
        >>> arr (setField "keywords" keywords)
        >>> arr (setField "bodyclass" "default")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireA "tags" (setFieldA "tags" (renderTagList'))
        >>> setFieldPageList (take 3 . recentFirst)
                "templates/postitem.html" "blogposts" "posts/blog/*"
        >>> setFieldPageList (take 5 . recentFirst)
                "templates/postitem.html" "projects" "posts/projects/*"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Tags
    create "tags" $
      requireAll "posts/*/*" (\_ ps -> readTags ps :: Tags String)
    
    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
        
    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*/*" >>> renderRss feedConfiguration
            
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render pages without relative url's
    forM_ ["404.md"] $ \p ->
        match p $ do
            route $ setExtension ".html"
            compile $ wunkiCompiler
                >>> applyTemplateCompiler "templates/default.html"

    -- Render pages with relative url's
    forM_ ["about.md"] $ \p ->
        match p $ do
            route $ setExtension ".html"
            compile $ wunkiCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

  where
      renderTagCloud' :: Compiler (Tags String) String
      renderTagCloud' = renderTagCloud tagIdentifier 100 120

      renderTagList' :: Compiler (Tags String) String
      renderTagList' = renderTagList tagIdentifier

      tagIdentifier :: String -> Identifier (Page String)
      tagIdentifier = fromCapture "tags/*"
    
      -- Common variables
      description = "etcet.net - homepage of Chris James"
      keywords = "chris, james, etcet, etcet.net, portfolio, linux, programming, chris james"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postitem.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" $ "Posts tagged " ++ tag)
        >>> arr (setField "description" $ "View all posts tagged with " ++ tag)
        >>> arr (setField "keywords" $ "etcet, tags, " ++ tag)
        >>> arr (setField "bodyclass" "postlist")
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

-- | Read a page, add default fields, substitute fields and render with Pandoc.
--
wunkiCompiler :: Compiler Resource (Page String)
wunkiCompiler = pageCompilerWith defaultHakyllParserState wunkiWriterOptions
        
-- | Custom HTML options for pandoc        
--
wunkiWriterOptions :: WriterOptions
wunkiWriterOptions = defaultHakyllWriterOptions
  { writerHtml5 = True
  , writerTableOfContents = True
  , writerLiterateHaskell = False
  }

config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                     \_site/* etcetn@etcet.net:/home/etcetn/www/"
  }
    
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "etcet.net"
    , feedDescription = "Homepage of Chris James"
    , feedAuthorName = "Chris James"
    , feedAuthorEmail = "cgjames@gmail.com"
    , feedRoot = "http://www.etcet.net"
    }

