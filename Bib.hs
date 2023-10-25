{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
module Bib (publicationList) where

import           Control.Applicative ((<|>))
import           Data.List           (sortOn)

import qualified Data.Text           as T

import           Text.BibTeX.Entry   (T (..))
import qualified Text.BibTeX.Parse   as P

import           Text.Parsec.String  (parseFromFile)

import           Text.Pandoc         (def, readLaTeX, runPure, writeMarkdown)

publicationList :: FilePath -> IO [String]
publicationList filename = do
    pubs <- readPubs filename
    return $ map format $ reverse $ sortOn (`field` "year") pubs

readPubs :: FilePath -> IO [T]
readPubs filename = do
    res <- parseFromFile P.file filename
    case res of (Left e) -> error $ "Parsec error in parsing .bib file " <> filename <> ":\n" <> show e
                (Right pubs) -> return pubs

format :: T -> String
format pub = case entryType pub of
    "article"     -> formatArticle pub
    "misc"        -> formatMisc pub
    "unpublished" -> formatUnpublished pub
    fmt           -> error $ "unsupported .bib entry format: " <> fmt

formatArticle :: T -> String
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
                        let issue = maybeToStr (fmap (\i -> "(" <> i <> ")") (maybeField pub "issue"))
                        return $ vol <> issue <> ": " <> pages <> ". ")
                  <> makelink (field pub "doi")

formatMisc :: T -> String
formatMisc pub =  field pub "author"
               <> ". ("
               <> field pub "year"
               <> ") \""
               <> field pub "title"
               <> ".\" "
               <> maybeToStr (fmap italicize (maybeField pub "journal" <|>
                                                maybeField pub "publisher"))
               <> makelink (field pub "doi")

formatUnpublished :: T -> String
formatUnpublished pub =  field pub "author"
                      <> ". ("
                      <> field pub "year"
                      <> ") \""
                      <> field pub "title"
                      <> ".\" "
                      <> maybeToStr (fmap texToMarkdown (maybeField pub "note"))
                      <> maybeToStr (fmap makelink (maybeField pub "doi"))


makelink :: String -> String
makelink s = "[" <> s <> "](https://doi.org/" <> s <> "). "

italicize :: String -> String
italicize s = "_" <> s <> "._ "

maybeToStr :: Maybe String -> String
maybeToStr (Just s) = s
maybeToStr Nothing  = ""

maybeField :: T -> String -> Maybe String
maybeField pub a = lookup a (fields pub)

field :: T -> String -> String
field pub a =
  case lookup a (fields pub) of
    Nothing -> error $ "bibliography error: cannot find field " <> a <> "in entry " <> identifier pub
    Just v -> v

texToMarkdown :: String -> String
texToMarkdown s =
    let result = runPure $ do
          x <- readLaTeX def (T.pack s)
          writeMarkdown def x
    in case result of
        Left e -> error $ "error reading latex commands in the string " <> show s <> ":\n" <> show e
        (Right s') -> T.unpack (T.strip s') <> ". "
