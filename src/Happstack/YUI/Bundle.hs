{-# LANGUAGE TemplateHaskell #-}

module Happstack.YUI.Bundle where

import Data.ByteString (ByteString)
import Data.FileEmbed  (embedDir)
import Data.Map        (Map, fromList)

bundle :: Map FilePath ByteString
bundle = fromList $(embedDir "bundle")
