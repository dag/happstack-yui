{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Happstack.Server.YUI
  ( implYUISite
  , bundle
  ) where

import Prelude hiding ((.))

import qualified Data.ByteString as B
import qualified Data.Map        as Map
import qualified Data.Text       as T

import Control.Category             (Category((.)))
import Control.Monad                (void, mzero)
import Data.List                    (intercalate)
import Data.Text.Encoding           (encodeUtf8)
import Happstack.Server             (ServerPartT, Response, neverExpires, setHeaderM, ok, toResponse, guessContentTypeM, mimeTypes, lookPairs)
import Happstack.Server.Compression (compressedResponseFilter)
import Happstack.Server.JMacro      ()
import Happstack.Server.YUI.Bundle  (bundle)
import Language.Javascript.JMacro   (JStat(BlockStat), jmacro, renderJs, jhFromList, toJExpr)
import Text.Boomerang.TH            (derivePrinterParsers)
import Text.PrettyPrint             (Style(mode), Mode(OneLineMode), renderStyle, style)
import Web.Routes                   (Site, RouteT, showURL)
import Web.Routes.Boomerang         (Router, (<>), (</>), rList, anyString, eos, boomerangSiteRouteT)
import Web.Routes.Happstack         (implSite)

#if !MIN_VERSION_template_haskell(2,7,0)
import Language.Javascript.JMacro   (JStat(..), JExpr(..), JVal(..), Ident(..))
#endif

data Sitemap
    = ComboHandlerURL
    | BundleURL [String]
    | ConfigURL
    | SeedURL

derivePrinterParsers ''Sitemap

sitemap :: Router Sitemap
sitemap =
    "3.5.1" </>                                                                 -- TODO: pass in YUI version via CPP from makefile?
       ( rComboHandlerURL . "combo"
      <> rBundleURL . "bundle" </> rList (anyString . eos)
      <> rConfigURL . "config"
      <> rSeedURL
       )

site :: Site Sitemap (ServerPartT IO Response)
site = boomerangSiteRouteT route sitemap

-- | Mounts a handler for serving YUI.
--
-- The handler responds to these routes:
--
-- [@\/3.5.1\/@]
--   The YUI seed file plus the configuration for using our own
--   combo loader.
--
-- [@\/3.5.1\/combo@]
--   The combo loader.
--
-- [@\/3.5.1\/bundle\/\<filename\>@]
--   Get an individual file without combo loading.
--
-- [@\/3.5.1\/config@]
--   The code for configuring YUI to use our own combo loader.  Not needed
--   if you use the seed file mentioned above.
--
-- The version number of the bundled YUI release is included in the routes
-- for sake of cache-busting: the routes all respond with far-future
-- expiration dates.
implYUISite :: T.Text  -- ^ The URL of your application, e.g. @\"http:\/\/localhost:8000\"@.
            -> T.Text  -- ^ The path under which to mount the YUI handler, e.g. @\"/yui\"@.
            -> ServerPartT IO Response
implYUISite domain approot = implSite domain approot site

mkConfig :: RouteT Sitemap (ServerPartT IO) JStat
mkConfig = do
    comboURL <- showURL ComboHandlerURL
    return [jmacro|
       YUI.applyConfig { comboBase: `((T.unpack comboURL) ++ "?")`, root: "" }
    |]

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = do
    neverExpires
    void compressedResponseFilter
    case url of
      BundleURL paths ->
        do let filepath = intercalate "/" paths
           mime <- guessContentTypeM mimeTypes filepath
           setHeaderM "Content-Type" mime
           maybe mzero (ok . toResponse) $ Map.lookup filepath bundle
      ComboHandlerURL ->
        do qs <- lookPairs
           mime <- guessContentTypeM mimeTypes (fst . head $ qs)
           setHeaderM "Content-Type" mime
           let combo = [ bundle Map.! q | (q,_) <- qs, Map.member q bundle ]    -- TODO: use Map.lookup instead of Map.member + Map.!
           if null combo                                                        -- TODO: maybe mzero also if a requested file isn't found
             then mzero                                                         --       (actually research how other combohandlers do error handling)
             else ok $ toResponse $ B.concat combo
      ConfigURL ->
        do config <- mkConfig
           ok $ toResponse config
      SeedURL ->
        do config <- mkConfig
           setHeaderM "Content-Type" "application/javascript"
           ok $ toResponse $ seed `B.append` (encode . render) config
  where
    seed   = bundle Map.! "yui/yui-min.js"
    render = renderStyle (style { mode = OneLineMode }) . renderJs
    encode = encodeUtf8 . T.pack
