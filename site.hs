--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-
 - Author     : Daniel Hillerström
 - Description: Personal/professional single, static page for research staff/students
 - 
 -}
import           Control.Monad       (liftM)
import           Data.Monoid         ((<>))
import           Data.Time           (getCurrentTime)
import           Data.Time.Format    (formatTime,defaultTimeLocale)
import           Hakyll
import           Prelude             hiding (id)
import qualified Text.Pandoc         as Pandoc





--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  -- | Static files
  match ("static/js/*.js" .||. "static/fonts/*" .||. "static/css/*.css" .||. "static/images/*.jpg" .||. "static/images/*.png") $ do
    route   idRoute
    compile copyFileCompiler

  -- | Publications
    match ("papers/*.pdf" .||. "papers/*.ps" .||. "papers/*.dvi" .||. "talks/*.pdf" .||. "talks/*.links") $ do
      route idRoute
      compile copyFileCompiler
    match "*.bib" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler

  -- | Sections
  match "sections/*" $ do
    route idRoute
    compile $ do
      let sectionCtx = pageContext
          
      defaultBibtexCompiler
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
          
      getResourceString
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/page.html" indexCtx
        >>= relativizeUrls

  -- | Templates
  match "templates/*" $ compile templateCompiler
--------------------------------------------------------------------------------
pageContext :: Context String
pageContext = mconcat
              [
                dateField "date" "%B %e, %Y"
              , constField "keywords" "computer science,mathematics,programming languages,compilers,parallelism"
              , constField "fullname" "Daniel Hillerstr&#246;m"
              , constField "position" "Researcher"                
              , constField "office" "5.28/1"
              , constField "calendarUrl" "https://www.google.com/calendar/embed?src=236h5mc80or5ltgj91acmr5vhag1s4jg%40import.calendar.google.com&amp;mode=WEEK&amp;ctz=Europe/London"
              , socialContext
              , defaultContext
              ]

socialContext :: Context String
socialContext = mconcat
                [
                  constField "url" "https://www.dhil.net/research"
                , constField "image" "https://www.dhil.net/research/static/images/danielhillerstrom.jpg"
                , constField "description" "Daniel Hillerstr&#246;m is a researcher at The University of Edinburgh."
                , constField "sitename" "Informatics research profile page"
                , constField "twitterHandle" "@dhillerstrom"
                , constField "pagetitle" "Daniel Hillerstr&#246;m | University of Edinburgh"
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

defaultBibtexCompiler :: Compiler (Item String)
defaultBibtexCompiler = bibtexCompiler "csl/ieee.csl" "papers.bib"

today :: IO String
today =
  do now <- getCurrentTime
     return $ formatTime defaultTimeLocale "%FT%T%QZ" now
