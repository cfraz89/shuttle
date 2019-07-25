{-# LANGUAGE FlexibleInstances #-}

module Shuttle.PathParam where

import qualified Data.Text as T

class PathParam x where
  fromParam :: T.Text -> x

instance PathParam Int where
  fromParam = read . T.unpack

instance PathParam String where
  fromParam = T.unpack