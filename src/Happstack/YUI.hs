{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Happstack.YUI where

import Prelude hiding ((.))

import qualified Data.ByteString as B
import qualified Data.Map        as Map
import qualified Data.Text       as T

import Control.Category             (Category((.)))
import Control.Monad                (void, mzero)
import Data.Text.Encoding           (encodeUtf8)
import Happstack.Server             (ServerPartT, Response, neverExpires, setHeaderM, ok, toResponse, guessContentTypeM, mimeTypes, lookPairs)
import Happstack.Server.Compression (compressedResponseFilter)
import Happstack.Server.JMacro      ()
import Language.Javascript.JMacro   (JStat(BlockStat), jmacro, renderJs, jhFromList, toJExpr)
import Text.Boomerang.TH            (derivePrinterParsers)
import Text.PrettyPrint             (Style(mode), Mode(OneLineMode), renderStyle, style)
import Web.Routes                   (Site, RouteT, showURL)
import Web.Routes.Boomerang         (Router, (<>), (</>), anyString, boomerangSiteRouteT)
import Web.Routes.Happstack         ()

import Happstack.YUI.Bundle

data Sitemap
    = ComboHandlerURL
    | BundleURL FilePath
    | ConfigURL
    | SeedURL

derivePrinterParsers ''Sitemap

sitemap :: Router Sitemap
sitemap =
    rComboHandlerURL . "combo"
      <>
    rBundleURL . "bundle" </> anyString
      <>
    rConfigURL . "config"
      <>
    rSeedURL

site :: Site Sitemap (ServerPartT IO Response)
site = boomerangSiteRouteT route sitemap

mkConfig :: RouteT Sitemap (ServerPartT IO) JStat
mkConfig = do
    comboURL <- showURL ComboHandlerURL
    return [jmacro|
       YUI.applyConfig { comboBase: `((T.unpack comboURL) ++ "?")` }
    |]

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = do
    neverExpires
    void compressedResponseFilter
    case url of
      BundleURL filepath ->
        do mime <- guessContentTypeM mimeTypes filepath
           setHeaderM "Content-Type" mime
           maybe mzero (ok . toResponse) $ Map.lookup filepath bundle
      ComboHandlerURL ->
        do qs <- lookPairs
           let combo = [ bundle Map.! q | (q,_) <- qs, Map.member q bundle ]  -- TODO: use Map.lookup instead of Map.member + Map.!
           setHeaderM "Content-Type" "application/javascript"                 -- TODO: guess content type from first file
           if null combo                                                      -- TODO: maybe mzero also if a requested file isn't found
             then mzero                                                       --       (actually research what other combohandlers do error handling)
             else ok $ toResponse $ B.concat combo
      ConfigURL ->
        do config <- mkConfig
           ok $ toResponse config
      SeedURL ->
        do config <- mkConfig
           ok $ toResponse $ seed `B.append` (encode . render) config
  where
    seed   = bundle Map.! "yui/yui-min.js"
    render = renderStyle (style { mode = OneLineMode }) . renderJs
    encode = encodeUtf8 . T.pack
