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

    -- Copy files
    match "patches/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy robots.txt
    match "robots.txt" $ do
      route   idRoute
      compile copyFileCompiler
          
    -- Render posts
    match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ wunkiCompiler
        >>> arr (renderDateField "date" "%Y-%m-%d" "Date unknown")
        >>> arr (setField "bodyclass" "post")
        >>> renderTagsField "prettytags" (fromCapture "tags/*")
        >>> applyTemplateCompiler "templates/post.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
      >>> arr (setField "title" "All posts")
      >>> arr (setField "bodyclass" "postlist")
      >>> requireAllA "posts/*" addPostList
      >>> applyTemplateCompiler "templates/posts.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
      >>> arr (setField "title" "A Few Bytes from Petar")
      >>> arr (setField "description" description)
      >>> arr (setField "keywords" keywords)
      >>> arr (setField "bodyclass" "default")
      >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
      >>> requireAllA "posts/*" (id *** arr (take 10 . reverse . sortByBaseName) >>> addPostList)
      >>> applyTemplateCompiler "templates/index.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
      requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
        
    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration
            
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render pages witouth relative url's
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

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"
    
      -- Common variables
    description = "A person's progression in the world of programming. Watch me how I stagger on the never-ending road to digital mastery."
    keywords = "petar, radosevic, wunki, bread and pepper, programming, haskell, freebsd, rest"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged with &#8216;" ++ tag ++ "&#8217;"))
        >>> arr (setField "description" ("View all posts tagged with " ++ tag))
        >>> arr (setField "keywords" ("wunki, tags, " ++ tag))
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
  { deployCommand = "rsync --checksum -ave 'ssh -p 22000' \
                     \_site/* wunki@141.138.137.36:/usr/local/www/wunki"
  }
    
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Wunki"
    , feedDescription = "A Few Bytes of Petar Radosevic"
    , feedAuthorName = "Petar Radosevic"
    , feedRoot = "http://www.wunki.org"
    }

