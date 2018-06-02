{-# LANGUAGE TemplateHaskell #-} -- Support code generation

module TweetsData where

import           Data.ByteString (ByteString) -- byte arrays
import qualified Data.FileEmbed as FileEmbed -- support code generation for file embedding

-- | The JSON tweet data stored as raw bytes
tweetsBytes :: ByteString
tweetsBytes = $(FileEmbed.embedFile "../data/tweets.json")
