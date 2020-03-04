{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Shuttle.TestRoute where

import Data.Symbol.Utils
import Shuttle.Core2

-- type Test = Route GET ("test" :/ Param "p1" Int :/ Param "p2" String :/ "test") Int

data Routes = TestRoute

instance ShuttleRoute 'TestRoute where 
  type Route 'TestRoute = GET "test/#{p1:Int}/#{p2:String}/test"
  type ResponseType 'TestRoute = Int

test :: IO ()
test = do
  process @'TestRoute "test.com/test/1/2/test" 
    $ \a b -> do
      print $ show a
      pure 0
  pure ()