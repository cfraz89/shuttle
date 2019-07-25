{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Shuttle.TestRoute where

import Shuttle.Core

type TestRoute = "test" :/ Param "p1" Int :/ Param "p2" String :/ "test" :-> Int

instance Handle TestRoute where
  handle a b = do
    print $ show a ++ ", " ++ b
    pure 0