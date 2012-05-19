{-# LANGUAGE TemplateHaskell #-}

module Happstack.YUI.Bundle where

import Data.ByteString (ByteString)
import Data.FileEmbed  (embedDir)
import Data.Map        (Map, fromList)

-- | Maps the filenames of the bundled YUI build to their contents.
bundle :: Map FilePath ByteString
bundle = fromList $(embedDir "bundle")
