-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Shuttle.Core where

-- import qualified Data.Text as T
-- import GHC.TypeLits
-- import GHC.Exts
-- import Debug.Trace

-- import Shuttle.PathParam

-- infixl 2 :/
-- data a :/ b

-- data Route (m :: Method) (r :: *) (a :: *)
-- data Method = GET | POST | PUT | DELETE | PATCH

-- data Static (name :: Symbol)
-- data Param (name :: Symbol) (a :: *)

-- -- Stub
-- type Response a = IO a

-- type family Tokens (path :: k) :: [*] where
--   Tokens (x :/ xs) = Tokens x ++ Tokens xs
--   Tokens (Param n x) = '[Param n x]
--   Tokens (x :: Symbol) = '[Static x]
--   Tokens x = TypeError (Text "Not a valid path segment: " :<>: ShowType x)

-- -- Given a path and handler function, break down the path, applying arguments to the handler to get a response
-- class HandlePath tokens a where
--   handlePath :: T.Text -> HandlerType tokens a -> Response a

-- instance (PathParam x, HandlePath xs a) => HandlePath (Param n x ': xs) a where
--   handlePath path handler = handlePath @xs segs $ handler (fromParam seg)
--     where (seg, segs) = splitPath path

-- instance HandlePath xs a => HandlePath (Static x ': xs) a where
--   handlePath path = handlePath @xs segs 
--     where (seg, segs) = splitPath path

-- instance HandlePath '[] a where
--   handlePath url handler = handler

-- splitPath :: T.Text -> (T.Text, T.Text)
-- splitPath path = (seg, T.dropWhile (== '/') segs)
--   where (seg, segs) = T.breakOn "/" path

-- -- Concat Type Lists
-- type family (++) (x :: [*]) (xs :: [*]) where
--   (a ': as) ++ xs = a ': (as ++ xs)
--   '[] ++ xs = xs

-- -- Return the handler function type for a list of tokens
-- type family HandlerType (tokens :: [*]) a where
--   HandlerType (Param n x ': xs) a = x -> HandlerType xs a
--   HandlerType (Static n ': xs) a = HandlerType xs a
--   HandlerType '[] a = Response a

-- class IsRoute r where
--   type RouteMethod r :: Method
--   type RoutePath r
--   type RouteResponse r

-- instance IsRoute (Route m p a) where
--   type RouteMethod (Route m p a) = m
--   type RoutePath (Route m p a) = p
--   type RouteResponse (Route m p a) = a
-- -- type family RoutePath route where
-- --   RoutePath (Route method p a) = p
-- --   RoutePath t = TypeError (ShowType t :$$: Text "Is not a route") 

-- -- type family RouteResponse route where
-- --   RouteResponse (Route method p a) = a
-- --   RouteResponse t = TypeError (ShowType t :$$: Text "Is not a route") 

-- type RouteTokens x = Tokens (RoutePath x)
-- type Handler x = HandlerType (RouteTokens x) (RouteResponse x)

-- class IsRoute route => Handle (route :: *) where
--   handle :: Handler route

-- process :: forall r. (Handle r, HandlePath (RouteTokens r) (RouteResponse r)) => T.Text -> Response (RouteResponse r)
-- process url = handlePath @(RouteTokens r) url (handle @r)
