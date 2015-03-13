--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid (mappend)
import           Hakyll hiding (renderTags)
import           System.Process
import           Text.HTML.TagSoup
import           Text.Parsec
import           Text.Pandoc
import           Text.Pandoc.Walk (walkM)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- Blog

    postType    "md"   postCompiler
    postType    "html" getResourceBody
    archivePage "Blog" blogCtx

    -- Essays

    inDir       "essays" essayCompile
    archivePage "Essays" essayArchCtx

    -- Top-level pages

    match "index.html" $ do
        idr $ do
            let elems = fmap (take 5) $ recentFirst =<< loadAll "blog/*"
                indexCtx =
                    listField "elems" postCtx elems `mappend`
                    defCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match (fromList ["contact.md"]) $ do
        route asHtml
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defCtx
            >>= relativizeUrls

    match "404.md" $ do
        idr $     getResourceBody
              >>= loadAndApplyTemplate "templates/default.html" defCtx

    -- Redirects old ocPortal URLs to Hakyll

    create ["index.php"] $ do
        idr $ do
            posts <- loadAll "blog/*"
            let redirectCtx = listField "posts" postCtx (return posts) `mappend`
                              defCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/redirect.html" redirectCtx

    -- Unfinished pages

    inDir "unfinished" essayCompile

    archivePage "Unfinished" (elems "unfinished" defCtx `mappend` defCtx)

    -- Assets

    cp "data/**"

    cp "images/**"

    cp "js/**"

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    -- Git repositories. These are maintained separately. You can populate the
    -- "git" directory using the "fetchGit" script

    cp "git/**"

    -- Feeds (not working yet)
    {-
    create ["atom.xml"] $ do
        idr $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst
                 =<< loadAllSnapshots "blog/*.md" "content"
            renderAtom feedConf feedCtx posts
    -}
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%F" `mappend` defCtx

defCtx = defaultContext

cp path = match path $ do
            route   idRoute
            compile copyFileCompiler

page cmp = do route $ setExtension "html"
              compile $ cmp
                  >>= postTmp

postTmp = loadAndApplyTemplate "templates/default.html" postCtx

asHtml = setExtension "html"

essayCompile =     compile $ postCompiler
               >>= loadAndApplyTemplate "templates/default.html" defCtx
               >>= relativizeUrls

feedConf :: FeedConfiguration
feedConf =  FeedConfiguration {feedTitle       = "Chris Warburton's Blog",
                               feedDescription = "Programming languages",
                               feedAuthorName  = "Chris Warburton",
                               feedAuthorEmail = "chriswarbo@gmail.com",
                               feedRoot        = "http://chriswarbo.net"}

renderPost p =     loadAndApplyTemplate "templates/post.html"    postCtx p
               >>= saveSnapshot "content"  -- for feeds
               >>= postTmp
               >>= relativizeUrls

inDir d c = match (fromGlob (d ++ "/**.md")) $ do
                route $ customRoute htmlExt
                  {-$ gsubRoute (d ++ "/") (const "") `composeRoutes` asHtml-}
                c

pandocFilter = withItemBody (unixFilter "pandoc" ["--mathml",
                                                  "--filter", "panpipe",
                                                  "--filter", "panhandle"])

postCompiler =     getResourceString
               >>= pandocFilter

strToLower = map toLower

archivePage title ctx = let name   = strToLower title
                            aCtx   = constField "title" title `mappend`
                                     ctx
                            path p = fromFilePath $ "templates/" ++ p ++ ".html"
                            temp t = loadAndApplyTemplate (path t) aCtx
                         in create [fromFilePath (name ++ ".html")] $ do
                                idr $ makeItem ""
                                    >>= temp name
                                    >>= temp "default"
                                    >>= relativizeUrls

elems d' c = let d  = strToLower d'
                 es = liftM concat $ mapM (loadAll . fromGlob . (d ++))
                                          ["/*", "/*/index.md"]
              in listField "elems" c es

postType t c = match (fromGlob ("blog/*." ++ t)) $ do
                   route asHtml
                   compile $ c >>= renderPost

idr x = route idRoute >> compile x

blogCtx =           listField "elems" postCtx (recentFirst =<< loadAll "blog/*")
          `mappend` defCtx

essayArchCtx = elems "essays" defCtx `mappend` defCtx

setExt e = (++ e) . reverse . dropWhile (/= '.') . reverse . toFilePath

htmlExt = setExt "html"
