{-# LANGUAGE TemplateHaskell #-}

module TweetsData where

import           Data.ByteString (ByteString)
import qualified Data.FileEmbed as FileEmbed

tweetsBytes :: ByteString
tweetsBytes = $(FileEmbed.embedFile "../data/tweets.json")
