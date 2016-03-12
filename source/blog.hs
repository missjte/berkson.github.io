{-# LANGUAGE OverloadedStrings #-}

import           Hakyll

import           Site.Sitemap

import           System.Environment
import           System.FilePath.Posix
import           System.Process

import           Control.Monad                   (forM)

import           Data.List                       (intersperse, isSuffixOf)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes)

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

host :: String
host = "http://prickyourfinger.org"

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { deployCommand             = "bash source/deploy.sh deploy"
  , providerDirectory         = "journal"
  , destinationDirectory      = "generated/deploy"
  , storeDirectory            = "generated/cache"
  , tmpDirectory              = "generated/tmp"
  , previewHost               = "0.0.0.0"
  , previewPort               = 4000
  , ignoreFile                = isIgnored
  }
  where
    isIgnored ".htaccess"     = False
    isIgnored name            = ignoreFile defaultConfiguration name

atomConfig :: FeedConfiguration
atomConfig = FeedConfiguration
  { feedTitle                 = "Prick Your Finger"
  , feedDescription           = "Latest blog posts from Eiren &amp; Berkson &#64; PrickYourFinger.org!"
  , feedAuthorName            = "Eiren &amp; Berkson"
  , feedAuthorEmail           = "eiren.berkson@gmail.com"
  , feedRoot                  = "http://prickyourfinger.org"
  }

sitemapConfig :: SitemapConfiguration
sitemapConfig = def { sitemapBase     = "http://prickyourfinger.org/" }

main :: IO ()
main = do

  (action:_) <- getArgs

  let preview = action == "watch" || action == "preview"
      hakyllConfig' = if preview
        then hakyllConfig { destinationDirectory = "generated/preview" }
        else hakyllConfig
      previewPattern stem =
        let normal = fromGlob $ stem ++ "/*"
            drafts = fromGlob "drafts/*"
        in if preview then normal .||. drafts else normal
      postsPattern = previewPattern "posts"

  hakyllWith hakyllConfig' $ do

    -- Build Tags
    tags <- buildTags postsPattern (fromCapture "tags/*/index.html")
    tagsRules tags $ \tag pattern -> do
      let title = "&ldquo;" ++ tag ++ "&rdquo;"
      route $ gsubRoute " " (const "-") `composeRoutes` setExtension "html"
      compile $ do
        list <- postList tags pattern recentFirst
        makeItem ""
          >>= loadAndApplyTemplate "template/tags.html" (mconcat [constField "body" list, archiveCtx tags, constField "tagged" title, defaultContext])
          >>= loadAndApplyTemplate "template/default.html" (mconcat [constField "title" title, defaultContext])
          >>= relativizeUrls
          >>= deIndexUrls

    -- Add static content
    match "static/*" $ do
      route   $ gsubRoute "static/" (const "")
      compile copyFileCompiler

    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "podcasts/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Add raw CSS
    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    -- Add Pages
    match "page/*" $ do
      route   $ setExtension "html" `composeRoutes`
                gsubRoute "page/" (const "") `composeRoutes`
                appendIndex
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "template/default.html" defaultContext
        >>= relativizeUrls
        >>= deIndexUrls

    -- Add Posts
    match postsPattern $ do
      route   $ setExtension "html" `composeRoutes`
                directorizeDate `composeRoutes`
                appendIndex
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "template/article.html"    (tagsCtx tags)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "template/default.html" (tagsCtx tags)
        >>= relativizeUrls
        >>= deIndexUrls

    -- Add Workouts
    match "workout/*" $ do
      route   $ setExtension "html" `composeRoutes`
                directorizeDateAndAuthor `composeRoutes`
                appendIndex
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "template/article.html"    (tagsCtx tags)
        >>= saveSnapshot "workout"
        >>= loadAndApplyTemplate "template/default.html" (tagsCtx tags)
        >>= relativizeUrls
        >>= deIndexUrls

    -- Generate Archive
    create ["archive"] $ do
      route   $ setExtension "html" `composeRoutes`
                appendIndex
      compile $ do
        posts <- loadAll postsPattern
        sorted <- recentFirst posts
        itemTpl <- loadBody "template/post-item.html"
        list <- applyTemplateList itemTpl listCtx sorted
        makeItem list
          >>= loadAndApplyTemplate "template/archive.html" (archiveCtx tags)
          >>= loadAndApplyTemplate "template/default.html" (archiveCtx tags)
          >>= relativizeUrls
          >>= deIndexUrls

    -- Generate Fitness Tracker
    create ["tracker"] $ do
      route   $ setExtension "html" `composeRoutes`
                appendIndex
      compile $ do
        workouts <- loadAll "workout/*"
        sorted <- recentFirst workouts
        itemTpl <- loadBody "template/post-item.html"
        list <- applyTemplateList itemTpl listCtx sorted
        makeItem list
          >>= loadAndApplyTemplate "template/archive.html" trackerCtx
          >>= loadAndApplyTemplate "template/default.html" trackerCtx
          >>= relativizeUrls
          >>= deIndexUrls

    -- Generate Homepage
    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- loadAll postsPattern
        sorted <- take 3 <$> recentFirst posts
        itemTpl <- loadBody "template/post-item.html"
        list <- applyTemplateList itemTpl listCtx sorted
        makeItem list
          >>= loadAndApplyTemplate "template/index.html"   (homeCtx list)
          >>= loadAndApplyTemplate "template/default.html" (homeCtx list)
          >>= relativizeUrls
          >>= deIndexUrls

    -- Generate Atom Feed for Fitness Tracker
    create ["tracker.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = mconcat [listCtx, bodyField "description"]
        workouts <- mapM deIndexUrls =<< fmap (take 10) . recentFirst
          =<< loadAllSnapshots "workout/*" "workout"
        renderAtom atomConfig feedCtx workouts

    -- Generate Atom Feed for Blog
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = mconcat [listCtx, bodyField "description"]
        posts <- mapM deIndexUrls =<< fmap (take 10) . recentFirst
          =<< loadAllSnapshots postsPattern "content"
        renderAtom atomConfig feedCtx posts

    -- Generate Sitemap
    create ["sitemap.xml"] $ do
      route   idRoute
      compile $ generateSitemap sitemapConfig
        >>= deIndexUrls

    -- Generate Templates
    match "template/*" $ compile templateCompiler

  where

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%B %e, %Y"
  , fileNameField "filename"
  , gitTag "git"
  , historyTag "history"
  , defaultContext
  ]

listCtx :: Context String
listCtx = mconcat
  [ dateField "date" "%d %b %Y"
  , fileNameField "filename"
  , gitTag "git"
  , historyTag "history"
  , defaultContext
  ]

archiveCtx :: Tags -> Context String
archiveCtx tags = mconcat
  [ constField "title" "Archive"
  , gitTag "git"
  , historyTag "history"
  , field "taglist" (\_ -> renderTagBlock tags)
  , defaultContext
  ]

trackerCtx :: Context String
trackerCtx = mconcat
  [ constField "title" "Fitness Tracker"
  , gitTag "git"
  , historyTag "history"
  , defaultContext
  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
  [ tagsBlock "prettytags" tags
  , constField "host" host
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
      renderHtml $ H.a ! A.class_ "button button-primary" ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")

tagsBlockWith :: (Identifier -> Compiler [String])
              -> String
              -> Tags
              -> Context a
tagsBlockWith getTags' key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag
        return $ renderLink tag route'

    return $ renderHtml $ mconcat $ intersperse " · " $ catMaybes links
  where
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
      H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

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

directorizeDate :: Routes
directorizeDate = gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

directorizeDateAndAuthor :: Routes
directorizeDateAndAuthor = metadataRoute $ \md ->
    gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ \s ->
        replaceAll "-" (const "/") s ++ (md M.! "author") ++ "/"

appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> p </> "index" <.> e) . splitExtension . toFilePath

stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) ("/." :: String)
  then take (length url - 10) url
  else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item
