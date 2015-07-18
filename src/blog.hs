{-# LANGUAGE OverloadedStrings #-}

import           Hakyll

import           System.FilePath.Posix
import           System.Process

import           Control.Monad                   (forM)
import           Data.Maybe                      (catMaybes)
import           Data.List                       (intersperse)


import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand = "bash src/deploy.sh deploy"
  , providerDirectory = "journal"
  , destinationDirectory = "generated/deploy/out"
  , storeDirectory = "generated/deploy/cache"
  , tmpDirectory = "generated/deploy/cache/tmp"
  , previewHost = "0.0.0.0"
  , previewPort = 4000
  , ignoreFile = isIgnored
  }
  where
    isIgnored path
      | ignoreFile defaultConfiguration name = True
      -- 4913 is a file vim creates on windows to verify
      -- that it can indeed write to the specified path
      | name == "4913"                         = True
      | otherwise                              = False
      where name = takeFileName path


atomFeedConf :: FeedConfiguration
atomFeedConf = FeedConfiguration
  { feedTitle = "Prick Your Finger"
  , feedDescription = "The latest blog posts from Eiren &amp; Berkson!"
  , feedAuthorName  = "Eiren &amp; Berkson"
  , feedAuthorEmail = "us@prickyourfinger.org"
  , feedRoot = "http://www.prickyourfinger.org"
  }

main :: IO ()
main = hakyllWith hakyllConf $ do
  -- Build tags
  tags <- buildTags "post/*" (fromCapture "tag/*.html")

  tagsRules tags $ \tag pattern -> do
    let
      title = "&ldquo;" ++ tag ++ "&rdquo;"
    route idRoute
    compile $ do
      list <- postList tags pattern recentFirst
      makeItem ""
        >>= loadAndApplyTemplate "template/archive.html" (mconcat [constField "body" list, archiveCtx tags, defaultContext])
        >>= loadAndApplyTemplate "template/default.html" (mconcat [constField "title" title, defaultContext])
        >>= relativizeUrls

  -- Add raw CSS
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  -- Add some default pages
  match (fromList ["about.md", "KISS.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "template/default.html" defaultContext
      >>= relativizeUrls

  -- Add static content
  mapM_ (`match` (route idRoute >> compile copyFileCompiler))
    [ "CNAME"
    , ".gitignore"
    , "*.png"
    , "favicon.ico"
    , "img/**"
    ]

  match (fromRegex "post/[^/]+\\.(md|org)$") $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "template/post.html"    (tagsCtx tags)
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "template/default.html" (tagsCtx tags)
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "post/*"
      sorted <- recentFirst posts
      itemTpl <- loadBody "template/post-item.html"
      list <- applyTemplateList itemTpl postCtx sorted
      makeItem list
        >>= loadAndApplyTemplate "template/archive.html" (archiveCtx tags)
        >>= loadAndApplyTemplate "template/default.html" (archiveCtx tags)
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "post/*"
      sorted <- take 3 <$> recentFirst posts
      itemTpl <- loadBody "template/post-item.html"
      list <- applyTemplateList itemTpl postCtx sorted
      makeItem list
        >>= loadAndApplyTemplate "template/index.html"   (homeCtx list)
        >>= loadAndApplyTemplate "template/default.html" (homeCtx list)
        >>= relativizeUrls

  match "template/*" $ compile templateCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let
        feedCtx = mconcat
          [ postCtx
          , bodyField "description"
          ]
      posts <- fmap (take 10)
        . recentFirst
        =<< loadAllSnapshots "post/*" "content"
      renderAtom atomFeedConf feedCtx posts
  where

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%d %b %Y"
  , fileNameField "filename"
  , gitTag "git"
  , historyTag "history"
  , defaultContext
  ]

archiveCtx :: Tags -> Context String
archiveCtx tags = mconcat
  [ constField "title" "Archive"
  , field "taglist" (\_ -> renderTagBlock tags)
  , defaultContext
  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
  [ tagsBlock "prettytags" tags
  , postCtx
  ]

homeCtx :: String -> Context String
homeCtx list = mconcat
  [ constField "post" list
  , constField "title" "Home"
  , defaultContext
  ]

renderTagBlock :: Tags -> Compiler String
renderTagBlock = renderTags makeLink unwords
  where
    makeLink tag url count _ _ =
      renderHtml $ H.code ! A.class_ "tags" $ H.a ! A.href (toValue url) $ toHtml (tag ++ "(" ++ show count ++ ")")

tagsBlockWith :: (Identifier -> Compiler [String])
              -> String
              -> Tags
              -> Context a
tagsBlockWith getTags' key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag
        return $ renderLink tag route'

    return $ renderHtml $ mconcat $ intersperse " " $ catMaybes links
  where
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
      H.code ! A.class_ "tags" $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

tagsBlock :: String
          -> Tags
          -> Context a
tagsBlock = tagsBlockWith getTags

postList :: Tags
  -> Pattern
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList tags pattern sortFilter = do
  posts <- sortFilter =<< loadAll pattern
  itemTpl <- loadBody "template/post-item.html"
  applyTemplateList itemTpl (tagsCtx tags) posts

fileNameField :: String -> Context String
fileNameField key = field key $ \item ->
  return . toFilePath $ itemIdentifier item

gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = "journal/" ++ toFilePath (itemIdentifier item)
      gitLog format =
        readProcess "git" [
          "log"
        , "-1"
        , "HEAD"
        , "--pretty=format:" ++ format
        , "--"
        , fp
        ] ""

  unsafeCompiler $ do
    sha     <- gitLog "%h"
    message <- gitLog "%s"

    let commit  = "https://github.com/berkson/berkson.github.io/commit/" ++ sha

    return $ if null sha
               then renderHtml $ H.code ! A.class_ "sha" $ H.string "Not Committed"
               else renderHtml $ H.code ! A.class_ "sha" $ H.a ! A.href (toValue commit) ! A.title (toValue message) $ toHtml sha

historyTag :: String -> Context String
historyTag key = field key $ \item -> do
  let fp = "journal/" ++ toFilePath (itemIdentifier item)
  let history = "https://github.com/berkson/berkson.github.io/commits/source/" ++ fp

  return . renderHtml $ H.code ! A.class_ "history" $ H.a ! A.href (toValue history) $ "History"
