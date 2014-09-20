--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Monoid (mappend)
import           Hakyll hiding (renderTags)
import           Text.HTML.TagSoup
import           Text.Parsec
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    cp "data/**"
    cp "images/**"
    cp "js/**"

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown"]) $ do
        route asHtml
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defCtx
            >>= relativizeUrls

    match "posts/*.markdown" $ do
        route asHtml
        compile $ pandocCompiler >>= renderPost

    match "posts/*.html" $ do
        route asHtml
        compile $ getResourceBody >>= renderPost

    match "posts/*.org" $ do
        route asHtml
        compile $     orgCompiler
                  >>= loadAndApplyTemplate "templates/default.html" postCtx
                  >>= relativizeUrls

    match "essays/**.md" $ do
        route $ gsubRoute "essays/" (const "") `composeRoutes` asHtml
        essayCompile

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField  "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"             `mappend`
                    defCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["essays.html"] $ do
        route idRoute
        compile $ do
            topLevel <- loadAll "essays/*"
            subLevel <- loadAll "essays/*/index.md"
            let essays   = topLevel ++ subLevel
                essayCtx =
                    listField  "essays" defCtx (return essays) `mappend`
                    constField "title"  "Essays"               `mappend`
                    defCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/essays.html"  essayCtx
                >>= loadAndApplyTemplate "templates/default.html" essayCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) $ recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField  "posts" postCtx (return posts) `mappend`
                    defCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- Redirect old URLs
    create ["index.php"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            let redirectCtx = listField "posts" postCtx (return posts) `mappend`
                              defCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/redirect.html" redirectCtx

    match "templates/*" $ compile templateCompiler

    match "404.html" $ do
        route idRoute
        compile $
            getResourceBody
                >>= applyAsTemplate                               defCtx
                >>= loadAndApplyTemplate "templates/default.html" defCtx
{-
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst
                 =<< loadAllSnapshots "posts/*.markdown" "content"
            renderAtom feedConf feedCtx posts
-}
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defCtx

defCtx = defaultContext

cp path = match path $ do
            route   idRoute
            compile copyFileCompiler

page cmp = do route $ setExtension "html"
              compile $ cmp
                  >>= loadAndApplyTemplate "templates/default.html" postCtx

asHtml = setExtension "html"

essayCompile =     compile $ pandocCompiler
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
               >>= loadAndApplyTemplate "templates/default.html" postCtx
               >>= relativizeUrls

orgCompiler :: Compiler (Item String)
orgCompiler = let render = unixFilter "./org2html.sh" [] in
              do orgBody <- getResourceBody
                 rendered <- render (itemBody orgBody)
                 let ctx = postCtx `mappend` titleField (htmlHeading rendered)
                     outBody = htmlBody rendered
                 loadAndApplyTemplate "templates/post.html" ctx
                                      (itemSetBody outBody orgBody)

tagOf (TagOpen t _) = t
tagOf _             = ""

getTag t = takeWhile (/= TagClose t) . dropWhile ((t /=) . tagOf)

stripTag t = takeWhile ((t /=) . tagOf) . dropWhile (/= TagClose t)

getAllTags t xs = let tag = getTag t xs in
                      if tag == [] then []
                                   else tag : getAllTags t (stripTag t xs)

appendTags :: [Tag String] -> [Tag String] -> [Tag String]
appendTags new old = init old ++ new ++ [last old]

htmlHeading = renderTags . getTag "h1" . parseTags

htmlBody s = let tags   = parseTags s
                 body   = getTag "body" tags
                 styles = concat $ getAllTags "style" tags
                 body'  = appendTags styles (stripTag "h1" body) in
                 renderTags body'
