
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Shuttle.Core2 where

import Data.Kind
import Data.Proxy
import Data.Symbol.Utils
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Exts
import Debug.Trace


import Shuttle.PathParam

type Response a = IO a

data MethodRoute where
  GET :: Symbol -> MethodRoute 
  POST :: Symbol -> MethodRoute

type family PathFor (r :: MethodRoute) where
  PathFor (GET s) = s
  PathFor (POST s) = s

data Param (name :: Symbol) (a :: Type)
data Static (name :: Symbol)

class ShuttleRoute (r :: k) where
  type Route r :: MethodRoute
  type ResponseType r :: Type

-- class (ShuttleRoute r, route ~ Route r, Listify route lst, tokens ~ ParseRoute lst, handler ~ HandlerType tokens a)  => HandlePath r a where
class HandlePath tokens a where
  handlePath :: T.Text -> HandlerType tokens a -> Response a

instance (PathParam x, HandlePath xs a) => HandlePath (Param n x ': xs) a where
  handlePath path handler = handlePath @xs segs $ handler (fromParam seg)
    where (seg, segs) = splitPath path

splitPath :: T.Text -> (T.Text, T.Text)
splitPath path = (seg, T.dropWhile (== '/') segs)
  where (seg, segs) = T.breakOn "/" path

instance (KnownSymbol x, HandlePath xs a) => HandlePath (Static x ': xs) a where
  handlePath path = handlePath @xs segs 
    where segs = T.splitOn (T.pack $ symbolVal @x Proxy) path !! 1

instance HandlePath '[] a where
  handlePath url handler = handler

type family ParseRoute (lst :: [Symbol]) :: [Type] where
  ParseRoute '[] = '[]
  ParseRoute ("#" ': x ': xs) = Param x Int ': ParseRoute xs
  ParseRoute (x ': xs) = ParseStatic x (ParseRoute xs)

type family ParseStatic (c :: Symbol) (lst :: [Type]) :: [Type] where
  ParseStatic c (Static s ': ss) = Static (AppendSymbol c s) ': ss
  ParseStatic c ss = Static c ': ss

-- Return the handler function type for a list of tokens
type family HandlerType (tokens :: [Type]) a where
  HandlerType (Param n x ': xs) a = x -> HandlerType xs a
  HandlerType (Static n ': xs) a = HandlerType xs a
  HandlerType '[] a = Response a

type Ls r chars = Listify (PathFor (Route r)) chars

process :: forall r chars tokens. (Ls r chars, tokens ~ ParseRoute chars, HandlePath tokens (ResponseType r)) => T.Text -> HandlerType tokens (ResponseType r) -> Response (ResponseType r)
process url handler = handlePath @tokens url handler
