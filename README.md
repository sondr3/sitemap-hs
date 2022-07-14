<h1 align="center">sitemap-hs</h1>
<p align="center">
    <a href="https://github.com/sondr3/sitemap-hs/actions"><img alt="GitHub Actions Status" src="https://github.com/sondr3/sitemap-hs/workflows/pipeline/badge.svg" /></a>
</p>

<p align="center">
    <b>A Haskell library for generating, parsing and validating sitemaps</b>
</p>

<details>
<summary>Table of Contents</summary>
<br />

## Table of Contents

- [Installation](#installation)
- [Getting started](#getting-started)
- [License](#license)
</details>

# Installation

Install via Hackage by adding it to your `.cabal`-file: 

```cabal
sitemap-hs ^>=0.1
```

Or install the CLI used to parse/validate sitemaps with `cabal install sitemap-hs`.

# Getting started

```haskell
import qualified Data.Text.Lazy as L
import Text.Sitemap (ChangeFrequency (..), SitemapEntry (..), newSitemap, nullSitemapEntry, renderSitemap)

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
```

# License

MIT or Apache-2.0.
