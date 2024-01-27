import Data.Text.Lazy qualified as L
import Text.Sitemap (ChangeFrequency (..), SitemapEntry (..), newSitemap, nullSitemapEntry, renderSitemap)

main :: IO ()
main = print sitemap

sitemap :: L.Text
sitemap =
  let entries =
        [ nullSitemapEntry "https://www.example.org",
          SitemapEntry
            { loc = "https://www.example.org/about/",
              priority = Just 0.8,
              lastModified = Nothing,
              changeFreq = Just Yearly
            }
        ]
   in renderSitemap $ newSitemap entries
