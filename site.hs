--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-
 - Author     : Daniel Hillerström
 - Description: Personal/professional single, static page for research staff/students
 - 
 -}
import           Control.Applicative ((<$>))
import           Control.Monad       (liftM)
import           Data.Monoid         ((<>), mconcat)
import           Prelude             hiding (id)
import qualified Text.Pandoc         as Pandoc
import Data.List (isPrefixOf, tails, findIndex, intercalate, sortBy)
import Data.Char (isSpace)
import Data.Time (getCurrentTime,toGregorian)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import           Hakyll
import           Hakyll.Web.Tags
import           Hakyll.Web.Pandoc.Biblio
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  -- | Static files
  match ("static/js/*.js" .||. "static/fonts/*" .||. "static/css/*.css" .||. "static/images/*.jpg" .||. "static/images/*.png") $ do
    route   idRoute
    compile copyFileCompiler

  -- | Publications
    match ("pubs/*.pdf" .||. "pubs/*.ps" .||. "pubs/*.dvi") $ do
      route idRoute
      compile copyFileCompiler
    match "publications.bib" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler

  -- | Sections
  match "sections/*" $ do
    route idRoute
    compile $ do
      let sectionCtx = pageContext
          
      bibtexCompiler "csl/ieee.csl" "publications.bib"
        >>= loadAndApplyTemplate "templates/section.html" sectionCtx
        >>= relativizeUrls
        >>= saveSnapshot "sections"

  -- | Main page
  match "index.tex" $ do
    route $ setExtension "html"
    compile $ do
      now <- unsafeCompiler today
      sections <- chronological =<< loadAllSnapshots "sections/*" "sections"
      let indexCtx = listField "sections" pageContext (return sections) <>
                     constField "modified" now <>
                     pageContext
          
      bibtexCompiler "csl/ieee.csl" "publications.bib"
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/page.html" indexCtx
        >>= relativizeUrls

  -- | Templates
  match "templates/*" $ compile templateCompiler
--------------------------------------------------------------------------------
pageContext = mconcat
              [
                dateField "date" "%B %e, %Y"
              , constField "keywords" "computer science,mathematics,programming languages,compilers,parallelism"
              , constField "fullname" "Daniel Hillerstr&#246;m"
              , constField "position" "Research Postgraduate Student"                
              , constField "description" "Research student at the University of Edinburgh"
              , constField "openGraphImage" "http://homepages.inf.ed.ac.uk/s1467124/static/images/danielhillerstrom.jpg"
              , constField "office" "1.07/2"
              , constField "calendarAddress" "https://www.google.com/calendar/embed?src=236h5mc80or5ltgj91acmr5vhag1s4jg%40import.calendar.google.com&amp;mode=WEEK&amp;ctz=Europe/London"
              , defaultContext
              ]

-- | Bibtex compiler
bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName
  = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM writePandoc
      (getResourceBody >>= readPandocBiblio Pandoc.def csl bib)

today :: IO String
today =
  do now <- getCurrentTime
     return $ formatTime defaultTimeLocale "%FT%T%QZ" now
