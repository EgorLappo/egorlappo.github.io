module Bib (publicationList) where

import Control.Applicative ((<|>))
import Data.List 

import qualified Data.Text as T

import Text.BibTeX.Entry
import qualified Text.BibTeX.Parse as P

import Text.Parsec.String

import Text.Pandoc

publicationList filename = do
    pubs <- readPubs filename
    return $ map format $ reverse $ sortOn (`field` "year") pubs

readPubs filename = do
    res <- parseFromFile P.file filename
    case res of (Left e) -> error $ "Parsec error in parsing .bib file " <> filename <> ":\n" <> show e 
                (Right pubs) -> return pubs

format pub = case entryType pub of 
    "article" -> formatArticle pub
    "misc"    -> formatMisc pub
    "unpublished" -> formatUnpublished pub

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
                       return $ vol <> ": " <> pages)
                  <> ". "
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
                      

    
makelink s = "[" <> s <> "](" <> s <> "). "
italicize s = "_" <> s <> "._ "

maybeToStr (Just s) = s
maybeToStr Nothing  = ""
 
maybeField pub a = lookup a (fields pub) 

field pub a = 
  case lookup a (fields pub) of
    Nothing -> error $ "bibliography error: cannot find field " <> a <> "in entry " <> identifier pub
    Just v -> v

texToMarkdown s = 
    let result = runPure $ do 
          x <- readLaTeX def (T.pack s) 
          writeMarkdown def x
    in case result of 
        Left e -> error $ "error reading latex commands in the string " <> show s <> ":\n" <> show e
        (Right s') -> T.unpack (T.strip s') <> ". "