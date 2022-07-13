module Text.Sitemap
  ( -- * Data structures
    SitemapEntry (..),
    Sitemap (..),
    SitemapIndex (..),
    ChangeFrequency (..),

    -- * Constructors
    nullSitemapEntry,
    newSitemap,
    newSitemapIndex,

    -- * Rendering
    buildSitemap,
    renderSitemap,
    renderSitemapWith,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import qualified Data.XML.Types as X
import qualified Text.XML as XML

data ChangeFrequency
  = Always
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  | Never
  deriving stock (Show, Eq, Ord)

changeFrequencyToText :: ChangeFrequency -> Text
changeFrequencyToText Always = "always"
changeFrequencyToText Hourly = "hourly"
changeFrequencyToText Daily = "daily"
changeFrequencyToText Weekly = "weekly"
changeFrequencyToText Monthly = "monthly"
changeFrequencyToText Yearly = "yearly"
changeFrequencyToText Never = "never"

data SitemapEntry = SitemapEntry
  { loc :: Text,
    lastModified :: Maybe UTCTime,
    changeFreq :: Maybe ChangeFrequency,
    priority :: Maybe Double
  }
  deriving stock (Show, Eq, Ord)

newtype Sitemap = Sitemap
  { urls :: [SitemapEntry]
  }
  deriving stock (Show, Eq, Ord)

newtype SitemapIndex = SitemapIndex
  { sitemaps :: [Sitemap]
  }
  deriving stock (Show, Eq, Ord)

nullSitemapEntry :: Text -> SitemapEntry
nullSitemapEntry url = SitemapEntry {loc = url, lastModified = Nothing, changeFreq = Nothing, priority = Nothing}

newSitemap :: [SitemapEntry] -> Sitemap
newSitemap urls = Sitemap {urls}

newSitemapIndex :: [Sitemap] -> SitemapIndex
newSitemapIndex sitemaps = SitemapIndex {sitemaps}

textNode :: X.Name -> Text -> X.Element
textNode name content = X.Element name [] [X.NodeContent $ X.ContentText content]

entryToXML :: SitemapEntry -> X.Node
entryToXML entry = X.NodeElement $ X.Element "url" [] (map X.NodeElement $ catMaybes [locXML, lastModXML, changeFreqXML, priorityXML])
  where
    locXML = Just $ textNode "loc" (loc entry)
    lastModXML = (\x -> Just $ textNode "lastmod" (T.pack $ formatShow iso8601Format x)) =<< lastModified entry
    changeFreqXML = (Just . textNode "changefreq" . changeFrequencyToText) =<< changeFreq entry
    priorityXML = (\x -> Just $ textNode "changeFreq" (T.pack $ show x)) =<< priority entry

buildSitemap :: Sitemap -> XML.Document
buildSitemap sitemap = case XML.fromXMLDocument (X.Document (X.Prologue [] Nothing []) urlset []) of
  Right doc -> doc
  Left _ -> error "malformed document"
  where
    urlset = X.Element "urlset" [("xmlns", ["http://www.sitemaps.org/schemas/sitemap/0.9"])] $ map entryToXML (urls sitemap)

renderSitemap :: Sitemap -> L.Text
renderSitemap = renderSitemapWith XML.def

renderSitemapWith :: XML.RenderSettings -> Sitemap -> L.Text
renderSitemapWith opts sitemap = XML.renderText opts (buildSitemap sitemap)
