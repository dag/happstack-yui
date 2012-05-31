{-# OPTIONS_GHC -F -pgmF trhsx #-}
{-# LANGUAGE FlexibleInstances, QuasiQuotes, TemplateHaskell #-}

module Main where

import Prelude hiding ((.), id)

import Control.Category
import Control.Monad
import Control.Monad.Trans
import Data.String
import Data.Unique
import HSX.JMacro
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.YUI
import Language.Javascript.JMacro
import Text.Boomerang.TH
import Web.Routes
import Web.Routes.Boomerang
import Web.Routes.Happstack
import Web.Routes.XMLGenT ()

instance IntegerSupply (RouteT Sitemap (ServerPartT IO)) where
    nextInteger = fmap (fromIntegral . (`mod` 1024) . hashUnique) (liftIO newUnique)

data Sitemap = YUI SitemapYUI | DemoURL

derivePrinterParsers ''Sitemap

sitemap :: Router Sitemap
sitemap = (rYUI . (lit "yui" </> sitemapYUI)) <> rDemoURL

site :: Site Sitemap (ServerPartT IO Response)
site = boomerangSiteRouteT route sitemap

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route (YUI url) = nestURL YUI (routeYUI url)
route DemoURL = do
    html <- unXMLGenT <h1>Set from <a href="http://yuilibrary.com/">YUI</a>!</h1>
    cssURL <- showURLParams (YUI CSSComboURL) [ (fromString "reset", Nothing)
                                              , (fromString "base", Nothing)
                                              , (fromString "fonts", Nothing)
                                              , (fromString "grids", Nothing)
                                              ]
    liftM toResponse $ unXMLGenT
      <html>
        <head>
          <link href=cssURL rel="stylesheet"/>
          <script src=(YUI SeedURL)/>
          <% [jmacro| YUI().use "node" \y -> y.one("h1").replace(`(y `createNode` html)`) |] %>
          <style>
            h1 { font-size: <% fontSize 36 %> }
          </style>
        </head>
        <body>
          <div class="yui3-g">
            <div class=(gridUnit 2 24)/>
            <div class="yui3-u">
              <h1>Boring unscripted title</h1>
            </div>
          </div>
        </body>
      </html>

main :: IO ()
main = simpleHTTP nullConf $
         implSite (fromString "http://localhost:8000") (fromString "") site
