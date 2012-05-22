{-# LANGUAGE TemplateHaskell #-}

module Happstack.Server.YUI.Bundle
  ( isYUIFile
  , readYUIFile
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed  (embedDir)
import Data.Map        (Map, (!), fromList, member)

bundle :: Map FilePath ByteString
bundle = fromList $(embedDir "bundle")

isYUIFile :: FilePath -> IO Bool
isYUIFile name = return $ member name bundle

readYUIFile :: FilePath -> IO ByteString
readYUIFile name = return $ bundle ! name
