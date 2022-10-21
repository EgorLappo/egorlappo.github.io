--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll

import qualified Text.Pandoc as P

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
        match "CNAME" $ do
            route   idRoute
            compile copyFileCompiler

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
            compile $ pandocCompilerWith blogReaderOptions blogWriterOptions
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
                posts <- fmap (take 5) (recentFirst =<< loadAll "posts/*")
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
    constField "description" "" `mappend`
    constField "twitter" "egor_lappo" `mappend`
    constField "github"  "egorlappo" `mappend`
    constField "pronunciation" "Ye-gór La-poe" `mappend`
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- https://gisli.hamstur.is/2020/08/my-personal-hakyll-cheatsheet/#katex-to-render-latex-math
blogReaderOptions :: P.ReaderOptions
blogReaderOptions = 
   defaultHakyllReaderOptions
      {
         P.readerExtensions = 
            P.readerExtensions defaultHakyllReaderOptions <> P.extensionsFromList
               [ 
                 P.Ext_tex_math_single_backslash  -- TeX math btw (..) [..]
               , P.Ext_tex_math_double_backslash  -- TeX math btw \(..\) \[..\]
               , P.Ext_tex_math_dollars           -- TeX math between $..$ or $$..$$
               , P.Ext_latex_macros               -- Parse LaTeX macro definitions (for math only)
               , P.Ext_inline_code_attributes     -- Ext_inline_code_attributes
               , P.Ext_abbreviations              -- PHP markdown extra abbreviation definitions
               ]
      }

blogWriterOptions :: P.WriterOptions
blogWriterOptions = P.def { P.writerHTMLMathMethod = P.MathJax "" }