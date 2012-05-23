{-# LANGUAGE CPP, TemplateHaskell #-}

module Happstack.Server.YUI.Bundle
  ( isYUIFile
  , readYUIFile
  ) where

import Data.ByteString (ByteString)

#if EMBED
import Data.FileEmbed            (embedDir)
import Data.Map                  (Map, (!), fromList, member)
#else
import Paths_happstack_yui       (getDataFileName)
import System.Directory          (doesFileExist)
import qualified Data.ByteString as B
#endif

#if EMBED
bundle :: Map FilePath ByteString
bundle = fromList $(embedDir "bundle")
#endif

-- | Tells if a file is included in the YUI bundle.
--
-- >>> isYUIFile "yui/yui-min.js"
-- True
isYUIFile :: FilePath -> IO Bool
#if EMBED
isYUIFile name = return $ member name bundle
#else
isYUIFile name = getDataFileName ("bundle/" ++ name) >>= doesFileExist
#endif

-- | Reads the contents of a file included in the YUI bundle.
readYUIFile :: FilePath -> IO ByteString
#if EMBED
readYUIFile name = return $ bundle ! name
#else
readYUIFile name = getDataFileName ("bundle/" ++ name) >>= B.readFile
#endif
