{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Lazy                   as M
import           Hakyll
import           System.FilePath.Posix
import           Text.Pandoc                     (HTMLMathMethod (MathJax),
                                                  WriterOptions (..))
import           Text.Pandoc.Definition

import           System.Process

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

main :: IO ()
main = hakyll $ do
  -- Build tags
  tags <- buildTags "post/*" (fromCapture "tag/*.html")

  tagsRules tags $ \tag pattern -> do
    let
      title = "&ldquo;" ++ tag ++ "&rdquo;"
    route idRoute
    compile $ do
      list <- postList tags pattern recentFirst
      makeItem ""
        >>= loadAndApplyTemplate "template/archive.html" (mconcat
          [ constField "body" list
          , archiveCtx tags
          , defaultContext
          ])
        >>= loadAndApplyTemplate "template/default.html" (mconcat
          [ constField "title" title
          , mathCtx
          , defaultContext
          ])
        >>= relativizeUrls

  -- Add raw CSS
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  -- Add Clay-based css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString
      >>= withItemBody
        (unixFilter "cabal" ["exec", "runghc"])

  -- Add some default pages
  match (fromList ["about.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "template/default.html" (mconcat
        [ mathCtx
        , defaultContext
        ])
      >>= relativizeUrls

  -- Add static content
  mapM_ (`match` (route idRoute >> compile copyFileCompiler))
    [ "CNAME"
    -- Although not tailored to the actual deployed site itself, it still
    -- has some rules that make it easier to git add/push new content.
    , ".gitignore"
    , "*.png"
    , "favicon.ico"
    , "img/**"
    ]

  match (fromRegex "post/[^/]+\\.(md|org)$") $ do
    route $ setExtension "html"
    compile $ pandocCompilerWithTransformM
      defaultHakyllReaderOptions
      pandocOptions
      transformer
      >>= loadAndApplyTemplate "template/post.html"    (tagsCtx tags)
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "template/default.html" (mconcat
        [ mathCtx
        , tagsCtx tags
        ])
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "post/*"
      sorted <- recentFirst posts
      itemTpl <- loadBody "template/post-item.html"
      list <- applyTemplateList itemTpl postCtx sorted
      makeItem list
        >>= loadAndApplyTemplate "template/archive.html"
          (archiveCtx tags)
        >>= loadAndApplyTemplate "template/default.html" (mconcat
          [ mathCtx
          , archiveCtx tags
          ])
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "post/*"
      sorted <- take 3 <$> recentFirst posts
      itemTpl <- loadBody "template/post-item.html"
      list <- applyTemplateList itemTpl postCtx sorted
      makeItem list
        >>= loadAndApplyTemplate "template/index.html" (homeCtx list)
        >>= loadAndApplyTemplate "template/default.html" (mconcat
          [ mathCtx
          , homeCtx list
          ])
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
  pandocOptions :: WriterOptions
  pandocOptions = defaultHakyllWriterOptions
    {writerHTMLMathMethod = MathJax ""}

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%Y-%m-%d"
  , fileNameField "filename"
  , gitTag "git"
  , defaultContext
  ]

archiveCtx :: Tags -> Context String
archiveCtx tags = mconcat
  [ constField "title" "Archive"
  , field "taglist" (\_ -> renderTagList tags)
  , defaultContext
  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
  [ tagsField "prettytags" tags
  , postCtx
  ]

homeCtx :: String -> Context String
homeCtx list = mconcat
  [ constField "post" list
  , constField "title" "Home"
  , defaultContext
  ]

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if M.member "mathjax" metadata then
    concat
      [ "<script src=\""
      , "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
      , "?config=TeX-AMS-MML_HTMLorMML\"></script>"
      ]
    else ""

gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = "post/" ++ toFilePath (itemIdentifier item)
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

    let history = "https://github.com/berkson/berkson.github.io/commits/source/" ++ fp
        commit  = "https://github.com/berkson/berkson.github.io/commit/" ++ sha

    return $ if null sha
               then "Not Committed"
               else renderHtml $ do
                      H.a ! A.href (toValue history) $ "History"
                      H.span ! A.class_ "hash" $ do
                        toHtml (", " :: String)
                        H.a ! A.href (toValue commit) ! A.title (toValue message) $ toHtml sha

postList
  :: Tags
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

transformer :: Pandoc -> Compiler Pandoc
transformer (Pandoc m bs0) = do
  bs1 <- mapM cbExpandRawInput bs0
  return . Pandoc m $ concat bs1

cbExpandRawInput :: Block -> Compiler [Block]
cbExpandRawInput block = case block of
  (BulletList xs) -> return . maybeBullets =<< mapM (mapM bList) xs
  _ -> return [block]
  where
  bList :: Block -> Compiler (Bool, Block)
  bList (Plain [Str "i", Space, Str fp]) = do
    let
      codeLang = case takeExtensions fp of
        ".c" -> ["c"]
        ".el" -> ["commonlisp"]
        ".hs" -> ["haskell"]
        ".rb" -> ["ruby"]
        ".sh" -> ["bash"]
        ".xorg.conf" -> ["xorg"]
        _ -> []
      httpTarget = "/code/" ++ fp
      fn = takeFileName fp
      attr = ("", "numberLines" : codeLang, [("input", "code/" ++ fp)])
    raw <- unsafeCompiler . readFile $ "code/" ++ fp
    return
      ( True
      ,
          Div ("", ["code-and-raw"], [])
          [ CodeBlock attr raw
          , Div ("", ["raw-link"], [])
            [ Plain
              [ RawInline
                "html" $
                unwords
                  [ "<a"
                  , " class=\"raw\""
                  , " href="
                  , dquote httpTarget
                  , " mimetype=text/plain"
                  , ">"
                  , fn
                  , "</a>"
                  ]
              ]
            ]
          ]
      )
  bList x = return (False, x)
  maybeBullets [] = [BulletList []]
  maybeBullets xss = case head xss of
    ((True, _):_) -> concatMap (map snd) xss
    _ -> [BulletList $ map (map snd) xss]

atomFeedConf :: FeedConfiguration
atomFeedConf = FeedConfiguration
  { feedTitle = "Prick Your Finger"
  , feedDescription = "The latest blog posts from Eiren & Berkson!"
  , feedAuthorName  = "Eiren & Berkson"
  , feedAuthorEmail = "us@prickyourfinger.org"
  , feedRoot = "http://www.prickyourfinger.org"
  }

dquote :: String -> String
dquote str = "\"" ++ str ++ "\""
