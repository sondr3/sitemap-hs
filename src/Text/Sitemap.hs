module Text.Sitemap
  ( -- * Data structures
    SitemapEntry (..),
    Sitemap (..),
    SitemapIndexEntry (..),
    SitemapIndex (..),
    ChangeFrequency (..),

    -- * Constructors
    nullSitemapEntry,
    newSitemap,
    newSitemapIndexEntry,
    newSitemapIndex,

    -- * Rendering sitemaps
    buildSitemap,
    renderSitemap,
    renderSitemapWith,

    -- * Rendering sitemap index
    buildSitemapIndex,
    renderSitemapIndex,
    renderSitemapIndexWith,
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
  deriving stock (Show, Eq, Ord, Read)

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
  deriving stock (Show, Eq, Ord, Read)

newtype Sitemap = Sitemap
  { urls :: [SitemapEntry]
  }
  deriving stock (Show, Eq, Ord, Read)

data SitemapIndexEntry = SitemapIndexEntry
  { sitemapLoc :: Text,
    sitemapLastModified :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Ord, Read)

newtype SitemapIndex = SitemapIndex
  { sitemaps :: [SitemapIndexEntry]
  }
  deriving stock (Show, Eq, Ord, Read)

nullSitemapEntry :: Text -> SitemapEntry
nullSitemapEntry url = SitemapEntry {loc = url, lastModified = Nothing, changeFreq = Nothing, priority = Nothing}

newSitemap :: [SitemapEntry] -> Sitemap
newSitemap urls = Sitemap {urls}

newSitemapIndexEntry :: Text -> SitemapIndexEntry
newSitemapIndexEntry loc = SitemapIndexEntry {sitemapLoc = loc, sitemapLastModified = Nothing}

newSitemapIndex :: [SitemapIndexEntry] -> SitemapIndex
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
    urlset =
      X.Element
        "urlset"
        [ ("xmlns", ["http://www.sitemaps.org/schemas/sitemap/0.9"]),
          ("xmlns:xsi", ["http://www.w3.org/2001/XMLSchema-instance"]),
          ("xsi:schemaLocation", ["http://www.sitemaps.org/schemas/sitemap/0.9 ", "http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"])
        ]
        $ map entryToXML (urls sitemap)

renderSitemap :: Sitemap -> L.Text
renderSitemap = renderSitemapWith XML.def

renderSitemapWith :: XML.RenderSettings -> Sitemap -> L.Text
renderSitemapWith opts sitemap = XML.renderText opts (buildSitemap sitemap)

sitemapEntryToXML :: SitemapIndexEntry -> X.Node
sitemapEntryToXML entry = X.NodeElement $ X.Element "sitemap" [] (map X.NodeElement $ catMaybes [locXML, lastModXML])
  where
    locXML = Just $ textNode "loc" (sitemapLoc entry)
    lastModXML = (\x -> Just $ textNode "lastmod" (T.pack $ formatShow iso8601Format x)) =<< sitemapLastModified entry

buildSitemapIndex :: SitemapIndex -> XML.Document
buildSitemapIndex sitemap = case XML.fromXMLDocument (X.Document (X.Prologue [] Nothing []) urlset []) of
  Right doc -> doc
  Left _ -> error "malformed document"
  where
    urlset =
      X.Element
        "sitemapindex"
        [ ("xmlns", ["http://www.sitemaps.org/schemas/sitemap/0.9"]),
          ("xmlns:xsi", ["http://www.w3.org/2001/XMLSchema-instance"]),
          ("xsi:schemaLocation", ["http://www.sitemaps.org/schemas/sitemap/0.9 ", "http://www.sitemaps.org/schemas/sitemap/0.9/siteindex.xsd"])
        ]
        $ map sitemapEntryToXML (sitemaps sitemap)

renderSitemapIndex :: SitemapIndex -> L.Text
renderSitemapIndex = renderSitemapIndexWith XML.def

renderSitemapIndexWith :: XML.RenderSettings -> SitemapIndex -> L.Text
renderSitemapIndexWith opts sitemap = XML.renderText opts (buildSitemapIndex sitemap)
