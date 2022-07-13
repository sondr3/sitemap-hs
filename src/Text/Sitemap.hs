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
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.XML.Types as XML

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

data Sitemap = Sitemap
  { urls :: [SitemapEntry]
  }
  deriving stock (Show, Eq, Ord)

data SitemapIndex = SitemapIndex
  { sitemaps :: [Sitemap]
  }
  deriving stock (Show, Eq, Ord)

sitemapNamespace :: Text
sitemapNamespace = "://www.sitemaps.org/schemas/sitemap/0.9"

nullSitemapEntry :: Text -> SitemapEntry
nullSitemapEntry url = SitemapEntry {loc = url, lastModified = Nothing, changeFreq = Nothing, priority = Nothing}

newSitemap :: [SitemapEntry] -> Sitemap
newSitemap urls = Sitemap {urls}

newSitemapIndex :: [Sitemap] -> SitemapIndex
newSitemapIndex sitemaps = SitemapIndex {sitemaps}
