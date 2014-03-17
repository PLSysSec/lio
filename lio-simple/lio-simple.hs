{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | The `lio-simple` utility for helping a user setup a lio-simple
-- web project, built on the Simple 'smpl' utility.
module Main (main) where

import Prelude hiding (writeFile, FilePath, all)
import Control.Applicative
import Control.Monad (when)
import Data.Aeson
import Data.Char
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.Encoding as T
import Data.Monoid (mempty)
import Data.Version
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Web.Simple.Templates.Language

import Paths_lio_simple

data LIOSimple = LIOSimple { appDir           :: FilePath
                           , includeTemplates :: Bool
                           , includeFS        :: Bool
                           , includeAll       :: Bool }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
  let lioSimple = 
        LIOSimple { appDir = "" &= argPos 0 &= typ "app_dir"
                  , includeTemplates = False
                            &= help "include templates"
                            &= explicit &= name "templates"
                  , includeFS = False
                            &= help "include lio-fs support"
                            &= explicit &= name "fs"
                  , includeAll = False
                            &= help
                                 "include templates and lio-fs support"
                            &= explicit &= name "all" }
                 &= help "Create a new application in app_dir"
                 &= program "lio-simple"
                 &= (summary $ "lio-simple " ++ showVersion version)
  (LIOSimple dir tmpls fs all) <- cmdArgs lioSimple
  createApplication dir (all || tmpls) (all || fs)

humanize :: String -> String
humanize = capitalize
  where go [] = []
        go ('_':xs) = ' ':(capitalize xs)
        go (x:xs) = x:(go xs)
        capitalize [] = []
        capitalize x@('_':_) = go x
        capitalize (x:xs) = (toUpper x):(go xs)

moduleCase :: String -> String
moduleCase = capitalize
  where go [] = []
        go ('_':xs) = capitalize xs
        go (x:xs) = x:(go xs)
        capitalize [] = []
        capitalize ('_':xs) = go xs
        capitalize (x:xs) = (toUpper x):(go xs)

createApplication :: FilePath -> Bool -> Bool -> IO ()
createApplication dir tmpls fs = do
  let myAppName = takeBaseName $ dropTrailingPathSeparator dir
      modName = moduleCase myAppName
      mappings = object
                  [ "appname" .= myAppName
                  , "name" .= humanize myAppName
                  , "module" .= modName
                  , "include_templates" .= tmpls
                  , "include_fs" .= fs ]

  createDirectory dir
  createDirectory $ dir </> modName
  copyTemplate ("template" </> "Main_hs.tmpl")
               (dir </> "Main.hs") mappings
  copyTemplate ("template" </> "Application_hs.tmpl")
               (dir </> "Application.hs") mappings
  copyTemplate ("template" </> "package_cabal.tmpl")
               (dir </> myAppName ++ ".cabal") mappings
  copyTemplate ("template" </> "Common_hs.tmpl")
               (dir </> modName </> "Common.hs") mappings

  when (tmpls || fs) $ createDirectory $ dir </> "liofs"
  when tmpls $ do
    createDirectory $ dir </> "liofs" </> "views"
    createDirectory $ dir </> "liofs" </> "layouts"
    copyTemplate ("template" </> "main_html.tmpl")
                 (dir </> "liofs" </> "layouts" </> "main.html") mappings
    copyTemplate ("template" </> "index_html.tmpl")
                 (dir </> "liofs" </> "views" </> "index.html") mappings

copyTemplate :: FilePath -> FilePath -> Value -> IO ()
copyTemplate orig target mappings = do
  etmpl <- compileTemplate <$> T.decodeUtf8 <$>
    (S8.readFile =<< getDataFileName orig)
  case etmpl of
    Left err -> fail err
    Right tmpl -> S8.writeFile target $ T.encodeUtf8 $
      renderTemplate tmpl mempty mappings

