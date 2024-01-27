module Main (main) where

import Data.Text.Lazy.IO qualified as TIO
import Options.Applicative
import Text.Sitemap (parseDocument)

data Options = Options
  { file :: String,
    verbose :: Bool
  }

options :: ParserInfo Options
options = info (helper <*> flags) (fullDesc <> progDesc "Sitemap generator" <> header "sitemap-generator")
  where
    flags =
      Options
        <$> strOption
          ( long "file"
              <> short 'f'
              <> metavar "FILE"
              <> help "File to read"
          )
        <*> switch
          ( long "verbose"
              <> short 'v'
              <> help "Enable verbose mode: verbosity level \"all\""
          )

run :: Options -> IO ()
run (Options file _) = do
  putStrLn $ "Reading file: " <> file
  content <- TIO.readFile file
  case parseDocument content of
    Nothing -> putStrLn "Error"
    Just doc -> print doc

main :: IO ()
main = run =<< execParser options
