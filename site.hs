--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll

import Bib (publicationList)

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = do 
    bibliography <- publicationList "pubs.bib" 
    hakyllWith config $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "files/*" $ do
            route    idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                -- >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/blogpost.html" postCtx
                >>= relativizeUrls

        match "blog.md" $ do
            route $ setExtension "html"
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let blogCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        staticCtx

                getResourceBody
                    >>= applyAsTemplate blogCtx
                    >>= renderPandoc
                    >>= loadAndApplyTemplate "templates/default.html" blogCtx
                    >>= relativizeUrls

        match "index.md" $ do
            route $ setExtension "html"
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let bibliography' = mapM makeItem bibliography
                    bibCtx = field "pub" (return . itemBody)
                let indexCtx =
                        listField "bibliography" bibCtx bibliography' `mappend`
                        listField "posts" postCtx (return posts) `mappend`
                        staticCtx

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= renderPandoc
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler

staticCtx :: Context String
staticCtx = 
    constField "email" "egor@ccrma.stanford.com" `mappend`
    constField "description" "PhD student,\nDepartment of Biology,\nStanford University" `mappend`
    constField "twitter" "egor_lappo" `mappend`
    constField "github"  "egorlappo" `mappend`
    constField "pronunciation" "Ye-gór La-poe" `mappend`
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

