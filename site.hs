--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Char
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    cp "data/**"
    cp "images/*"
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
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*.html" $ do
        route asHtml
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "essays/*.md" $ do
        route $ gsubRoute "essays/" (const "") `composeRoutes` asHtml
        essayCompile

    match "essays/*/*.md" $ do
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
                    --constField "title" "Home"                 `mappend`
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

essayCompile = compile $ pandocCompiler
               >>= loadAndApplyTemplate "templates/default.html" defCtx
               >>= relativizeUrls
