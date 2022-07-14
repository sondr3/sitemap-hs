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

    -- * Parsing
    parseSitemap,
  )
where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time (UTCTime, ZonedTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import qualified Data.XML.Types as X
import Text.XML (ParseSettings (psRetainNamespaces))
import qualified Text.XML as XML

data ChangeFrequency
  = Always
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  | Never
  deriving stock (Show, Read)

changeFrequencyToText :: ChangeFrequency -> Text
changeFrequencyToText Always = "always"
changeFrequencyToText Hourly = "hourly"
changeFrequencyToText Daily = "daily"
changeFrequencyToText Weekly = "weekly"
changeFrequencyToText Monthly = "monthly"
changeFrequencyToText Yearly = "yearly"
changeFrequencyToText Never = "never"

data ModifiedTime
  = ModifiedUTC UTCTime
  | ModifiedZoned ZonedTime
  deriving stock (Show, Read)

data SitemapEntry = SitemapEntry
  { loc :: Text,
    lastModified :: Maybe ModifiedTime,
    changeFreq :: Maybe ChangeFrequency,
    priority :: Maybe Double
  }
  deriving stock (Show, Read)

newtype Sitemap = Sitemap
  { urls :: [SitemapEntry]
  }
  deriving stock (Show, Read)

data SitemapIndexEntry = SitemapIndexEntry
  { sitemapLoc :: Text,
    sitemapLastModified :: Maybe ModifiedTime
  }
  deriving stock (Show, Read)

newtype SitemapIndex = SitemapIndex
  { sitemaps :: [SitemapIndexEntry]
  }
  deriving stock (Show, Read)

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

locXML :: Text -> Maybe X.Element
locXML l = Just $ textNode "loc" l

lastModXML :: ModifiedTime -> Maybe X.Element
lastModXML (ModifiedUTC time) = Just $ textNode "lastmod" (T.pack $ formatShow iso8601Format time)
lastModXML (ModifiedZoned time) = Just $ textNode "lastmod" (T.pack $ formatShow iso8601Format time)

changeFreqXML :: ChangeFrequency -> Maybe X.Element
changeFreqXML = Just . textNode "changefreq" . changeFrequencyToText

priorityXML :: Double -> Maybe X.Element
priorityXML prio = Just $ textNode "priority" (T.pack $ show prio)

entryToXML :: SitemapEntry -> X.Node
entryToXML entry =
  X.NodeElement $
    X.Element
      "url"
      []
      ( map X.NodeElement $
          catMaybes
            [ locXML (loc entry),
              lastModXML =<< lastModified entry,
              changeFreqXML =<< changeFreq entry,
              priorityXML =<< priority entry
            ]
      )

document :: X.Element -> Either (Set Text) XML.Document
document body = XML.fromXMLDocument $ X.Document (X.Prologue [] Nothing []) body []

rootXML :: XML.Name -> Text -> [X.Node] -> X.Element
rootXML name kind =
  X.Element
    name
    [ ("xmlns", ["http://www.sitemaps.org/schemas/sitemap/0.9"]),
      ("xmlns:xsi", ["http://www.w3.org/2001/XMLSchema-instance"]),
      ("xsi:schemaLocation", ["http://www.sitemaps.org/schemas/sitemap/0.9 ", schema])
    ]
  where
    schema = X.ContentText $ "http://www.sitemaps.org/schemas/sitemap/0.9/" <> kind <> ".xsd"

buildSitemap :: Sitemap -> XML.Document
buildSitemap sitemap = case document (rootXML "urlset" "sitemap" (map entryToXML $ urls sitemap)) of
  Right doc -> doc
  Left _ -> error "malformed document"

renderSitemap :: Sitemap -> L.Text
renderSitemap = renderSitemapWith XML.def

renderSitemapWith :: XML.RenderSettings -> Sitemap -> L.Text
renderSitemapWith opts sitemap = XML.renderText opts (buildSitemap sitemap)

parseDocument :: L.Text -> Maybe X.Document
parseDocument doc = case XML.parseText (XML.def {psRetainNamespaces = True}) doc of
  Right d -> Just $ XML.toXMLDocument d
  Left _ -> Nothing

parseSitemap :: L.Text -> Maybe Sitemap
parseSitemap doc = case parseDocument doc of
  Just d -> Just undefined
  Nothing -> Nothing

sitemapEntryToXML :: SitemapIndexEntry -> X.Node
sitemapEntryToXML entry = X.NodeElement $ X.Element "sitemap" [] (map X.NodeElement $ catMaybes [locXML $ sitemapLoc entry, lastModXML =<< sitemapLastModified entry])

buildSitemapIndex :: SitemapIndex -> XML.Document
buildSitemapIndex sitemap = case document (rootXML "sitemapindex" "siteindex" (map sitemapEntryToXML $ sitemaps sitemap)) of
  Right doc -> doc
  Left _ -> error "malformed document"

renderSitemapIndex :: SitemapIndex -> L.Text
renderSitemapIndex = renderSitemapIndexWith XML.def

renderSitemapIndexWith :: XML.RenderSettings -> SitemapIndex -> L.Text
renderSitemapIndexWith opts sitemap = XML.renderText opts (buildSitemapIndex sitemap)
