---
title: Manually formatting a publication list with Haskell
subtitle: List publications on a personal page with Hakyll and Pandoc
description: |
    In this post I show a way to manually transform contents 
    of a bibliography file into a nicely formatted markdown with Haskell 
---

While setting up my personal [page](https://egorlappo.github.io) with Hakyll, I have discovered that there is no simple copy-pastable solution for generating a list of publications from a `.bib` file. In this post I show a way to manually transform contents of a bibliography file into a nicely formatted markdown. You can copy my code and very quickly adapt it to your needs. The final result is available [on github](https://github.com/EgorLappo/egorlappo.github.io/blob/master/Bib.hs).

When it comes to references, Pandoc does have built-in citation processing machinery, and it can be used with Hakyll to cite works in blog posts (see, for example, [this guide](https://github.com/jaspervdj/hakyll-citeproc-example)). As for obtaining a plain publication list, the main suggestion seems to be to use an empty file containing a `\nocite{*}` command to make Pandoc list the references. 

However, I have found that this method is impossible to customize! The basic requirements are: reverse chronological sorting of references and no "unique names", which means that repeated author combinations should be written out in full. With biblatex, this can be solved with a combination of settings:

```latex
\usepackage[backend=biber, style=numeric, sorting=ydnt, firstinits=true, uniquename=false]{biblatex}
```

With Pandoc, this problem seems to require a manual approach. I will show the basic code to process a personal bibliography. 

# Setup

Make sure that your project includes the following dependencies from Hackage: `text`, `pandoc`, `parsec`, `bibtex`. The `bibtex` package provides unsophisticated parsers of `.bib` files, which is exactly what we need. 

# Code 

Let's begin with some necessary imports:
```haskell
module Bib (publicationList) where

import Control.Applicative ( (<|>) )
import Data.List ( sortOn ) 
import qualified Data.Text as T

-- contains the type of a bibtex entry
import Text.BibTeX.Entry ( T(..) )
-- contains bibtex parsers
import qualified Text.BibTeX.Parse as P

import Text.Parsec.String ( parseFromFile )

import Text.Pandoc ( runPure, readLaTeX, writeMarkdown, def )
```

Now, let's write the main logic of the module: the function to read the file, run the parser, and call a proper formatter on each entry.
```haskell
publicationList :: FilePath -> IO [String]
publicationList filename = do
    -- read and parse bibtex entries
    pubs <- readPubs filename
    -- descending sort by year, then format and return
    return $ map format $ reverse $ sortOn (`field` "year") pubs

-- runs a parser P.file on a fiven filename
-- panics when there is a parsing error
readPubs filename = do
    res <- parseFromFile P.file filename
    case res of 
      (Left e) -> 
        error $ "Parsec error in parsing .bib file " 
              <> filename <> ":\n" <> show e 
      (Right pubs) -> return pubs

-- reads the entry type 
-- and then calls the corresponding formatter
format pub = case entryType pub of 
    "article" -> formatArticle pub
    "misc"    -> formatMisc pub
    "unpublished" -> formatUnpublished pub
    fmt       -> error $ "unsupported .bib entry format: " <> fmt
```

Finally, let's write some formatters! For my page, I have settled on roughly the following format: first go the authors, then the year, then the title in quotation marks, and then maybe journal information and a URL. Of course, this is mostly arbitrary.

We need several convenience functions first. The fields of the entry are parsed into an association list, so we can adapt `lookup` to get their values. 

```haskell
-- for mandatory fields: 
-- throw an error if not present
field :: T -> String -> String
field pub a = 
  case lookup a (fields pub) of
    Nothing -> 
        error $ "bibliography error: cannot find field " 
              <> a <> "in entry " <> identifier pub
    Just v -> v

-- for optional fields
maybeField :: T -> String -> Maybe String
maybeField pub a = lookup a (fields pub) 
```

Then, some extra embellishment functions. The most important is ``texToMarkdown``, which converts any LaTeX syntax to Markdown by passing the string through Pandoc. I use it to render the "notes" field for some entries, but it can potentially be used for any field to get rid of diacritics or other syntax.

```haskell 
texToMarkdown :: String -> String
texToMarkdown s = 
    let result = runPure $ do 
          x <- readLaTeX def (T.pack s) 
          writeMarkdown def x
    in case result of 
        Left e -> 
            error $ "error reading latex commands in the string " 
                  <> show s <> ":\n" <> show e
        (Right s') -> T.unpack (T.strip s') <> ". "

makelink s = "[" <> s <> "](" <> s <> "). "

italicize s = "_" <> s <> "._ "

maybeToStr (Just s) = s
maybeToStr Nothing  = ""
```

Finally, here is how we can implement the formatters:

```haskell 
formatArticle pub =  field pub "author" 
                  <> ". ("
                  <> field pub "year"
                  <> ") \"" 
                  <> field pub "title"
                  <> ".\" "
                  <> italicize (field pub "journal")
                  <> maybeToStr (do
                       vol <- maybeField pub "volume"
                       pages <- maybeField pub "pages"
                       return $ vol <> ": " <> pages <> ". ")
                  <> makelink (field pub "doi")
                  
formatMisc pub =  field pub "author" 
               <> ". ("
               <> field pub "year"
               <> ") \"" 
               <> field pub "title"
               <> ".\" "
               <> maybeToStr (fmap italicize (maybeField pub "journal" <|> 
                                                maybeField pub "publisher"))
               <> makelink (field pub "doi")

formatUnpublished pub =  field pub "author" 
                      <> ". ("
                      <> field pub "year"
                      <> ") \"" 
                      <> field pub "title"
                      <> ".\" "
                      <> maybeToStr (fmap texToMarkdown (maybeField pub "note"))
                      <> maybeToStr (fmap makelink (maybeField pub "doi"))
```

I have tried to deal with optional fields in a "monadic" way, by using `fmap` and `<|>` operators, and I think it looks acceptable in the end. 

# Use it yourself

The source file for this post is available [here](https://github.com/EgorLappo/egorlappo.github.io/blob/master/Bib.hs). A result of runnning this script can be seen at [my homepage](https://egorlappo.github.io) ([source](https://github.com/EgorLappo/egorlappo.github.io/)).

To use it with Hakyll, you can use a `listField` to pass a list of references into a template. In the main file, you can do something like this: 
```haskell
main = do 
    bibliography <- publicationList "pubs.bib" 
    hakyllWith config $ do 
        ...

        match "index.md" $ do
            route $ setExtension "html"
            compile $ do
                ...
                let bibliography' = mapM makeItem bibliography
                    bibCtx = field "pub" (return . itemBody)
                let ctx =
                    listField "bibliography" bibCtx bibliography' `mappend`
                    ...
                ...
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= renderPandoc
                    ...
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls
```

Please reach out to me with comments and suggestions!