{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module: Text.Sitemap
-- Description: Sitemap generation, parsing and validation
-- Copyright: (c) Sondre Aasemoen
-- SPDX-License-Identifier: (MIT OR Apache-2.0)
--
-- Maintainer: Sondre Aasemoen <sondre@eons.io>
-- Stability: experimental
-- Portability: portable
--
-- Parsing, validating and generating sitemaps.
--
-- == Usage
--
-- To use this library you need to add it as a dependency in your projuct, e.g.
-- by adding it to @build-depends@ in your @.cabal@ file.
--
-- @
-- build-depends: sitemap-hs ^>= 0.1.0
-- @
--
-- This module does not use common names and can be directly imported:
--
-- @
-- __import__ Text.Sitemap
-- @
module Text.Sitemap
  ( -- | Sitemaps are an easy way for webmasters to inform search engines about pages on their sites that are available for crawling. In its simplest form, a Sitemap is an XML file that lists URLs for a site along with additional metadata about each URL (when it was last updated, how often it usually changes, and how important it is, relative to other URLs in the site) so that search engines can more intelligently crawl the site.
    --
    -- See the [website](https://sitemaps.org/index.html) for more information.

    -- * Data structures
    SitemapEntry (..),
    Sitemap (..),
    SitemapIndexEntry (..),
    SitemapIndex (..),
    ChangeFrequency (..),
    ModifiedTime (..),

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

    -- parseSitemap,

    -- * Utilities
    prettyXML,
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
import Text.XML (ParseSettings (psRetainNamespaces), RenderSettings (rsPretty))
import qualified Text.XML as XML

-- | How frequently the page is likely to change.
data ChangeFrequency
  = -- | The document changes on every visit.
    Always
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  | -- | The page is effectively archived.
    Never
  deriving stock (Show, Read)

changeFrequencyToText :: ChangeFrequency -> Text
changeFrequencyToText Always = "always"
changeFrequencyToText Hourly = "hourly"
changeFrequencyToText Daily = "daily"
changeFrequencyToText Weekly = "weekly"
changeFrequencyToText Monthly = "monthly"
changeFrequencyToText Yearly = "yearly"
changeFrequencyToText Never = "never"

-- | Date format used for when a page was last modified.
data ModifiedTime
  = -- | The timezone for this page is in UTC
    ModifiedUTC UTCTime
  | -- | The timezone for this page has a time zone
    ModifiedZoned ZonedTime
  deriving stock (Show, Read)

-- | A single page on your website.
data SitemapEntry = SitemapEntry
  { -- | URL of the page
    loc :: Text,
    -- | Date of the last modification of the page
    lastModified :: Maybe ModifiedTime,
    -- | How frequently the page chages. See 'ChangeFrequency'
    changeFreq :: Maybe ChangeFrequency,
    -- | The priority of the page relative to other URLs
    priority :: Maybe Double
  }
  deriving stock (Show, Read)

-- | A sitemap.
newtype Sitemap = Sitemap
  { -- | All the URLs in this sitemap
    urls :: [SitemapEntry]
  }
  deriving stock (Show, Read)

-- | A link to a sitemap, for when you require many sitemaps.
data SitemapIndexEntry = SitemapIndexEntry
  { -- | URL of the sitemap
    sitemapLoc :: Text,
    -- | When the sitemap was last modified
    sitemapLastModified :: Maybe ModifiedTime
  }
  deriving stock (Show, Read)

-- | An index of all sitemaps on the page.
newtype SitemapIndex = SitemapIndex
  { -- | A list of all sitemaps
    sitemaps :: [SitemapIndexEntry]
  }
  deriving stock (Show, Read)

-- | Create a new URL entry from its link
--
-- ==== __Examples__
--
-- >>> nullSitemapEntry "https://www.example.org"
-- SitemapEntry {loc = "https://www.example.org", lastModified = Nothing, changeFreq = Nothing, priority = Nothing}
--
-- @since 0.1.0.0
nullSitemapEntry :: Text -> SitemapEntry
nullSitemapEntry url = SitemapEntry {loc = url, lastModified = Nothing, changeFreq = Nothing, priority = Nothing}

-- | Create a sitemap from a list of URLs.
--
-- ==== __Examples__
--
-- >>> newSitemap [nullSitemapEntry "https://www.example.org"]
-- Sitemap {urls = [SitemapEntry {loc = "https://www.example.org", lastModified = Nothing, changeFreq = Nothing, priority = Nothing}]}
--
-- @since 0.1.0.0
newSitemap :: [SitemapEntry] -> Sitemap
newSitemap urls = Sitemap {urls}

-- | Create a new sitemap entry from its link
--
-- @since 0.1.0.0
newSitemapIndexEntry :: Text -> SitemapIndexEntry
newSitemapIndexEntry loc = SitemapIndexEntry {sitemapLoc = loc, sitemapLastModified = Nothing}

-- | Create a sitemap index from a list of sitemaps.
--
-- @since 0.1.0.0
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

-- | Convert a sitemap into a XML document
--
-- @since 0.1.0.0
buildSitemap :: Sitemap -> XML.Document
buildSitemap sitemap = case document (rootXML "urlset" "sitemap" (map entryToXML $ urls sitemap)) of
  Right doc -> doc
  Left _ -> error "malformed document"

-- | Pretty formatting for XML documents
--
-- @since 0.1.0.0
prettyXML :: RenderSettings
prettyXML = XML.def {rsPretty = True}

-- | Convert a sitemap into a rendered, minified XML text
--
-- @since 0.1.0.0
renderSitemap :: Sitemap -> L.Text
renderSitemap = renderSitemapWith XML.def

-- | Convert a sitemap into a rendered XML text, but with formatting options
--
-- @since 0.1.0.0
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

-- | Convert a sitemap index into a XML document
--
-- @since 0.1.0.0
buildSitemapIndex :: SitemapIndex -> XML.Document
buildSitemapIndex sitemap = case document (rootXML "sitemapindex" "siteindex" (map sitemapEntryToXML $ sitemaps sitemap)) of
  Right doc -> doc
  Left _ -> error "malformed document"

-- | Convert a sitemap index into a rendered, minified XML text
--
-- @since 0.1.0.0
renderSitemapIndex :: SitemapIndex -> L.Text
renderSitemapIndex = renderSitemapIndexWith XML.def

-- | Convert a sitemap index into a rendered XML text, but with formatting options
--
-- @since 0.1.0.0
renderSitemapIndexWith :: XML.RenderSettings -> SitemapIndex -> L.Text
renderSitemapIndexWith opts sitemap = XML.renderText opts (buildSitemapIndex sitemap)
