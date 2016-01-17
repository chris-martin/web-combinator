-- Webapps for which we can generate responses without side effects.
module WebCombinator.Pure
    ( App(..)
    , Body
    , emptyBody
    , emptyResponse
    , resolve
    , staticFiles
    ) where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Prelude (($), (==), (/=), Monoid(..), otherwise)
import WebCombinator.Message
import WebCombinator.Util (mapEntries, mapEntriesMaybe)

type Body = ByteString

emptyBody :: Body
emptyBody = ByteString.empty

response :: Status -> Body -> (Response, Body)
response s b = (Response { responseStatus = s, responseHeaders = [] }, b)

emptyResponse :: Status -> (Response, Body)
emptyResponse s = response s emptyBody

type StaticMapping = Map Path (Response, Body)

type StaticFilesMapping = Map PathSegments Body

data App =
    -- The 'Empty' app handles every request with a minimal not-found response.
    Empty
    -- 'Static' paths match exactly, including query string.
    | Static StaticMapping
    -- 'StaticFiles' paths do not contain a query string, and a server ignores
    -- any query string it receives from a client.
    | StaticFiles StaticFilesMapping

staticFilesToStatic :: StaticFilesMapping -> StaticMapping
staticFilesToStatic mapping = mapEntries f mapping
    where f (pathSegments, body) = ( (pathSegments, Nothing)
                                   , response status200 body)

fallback Empty x = x
fallback x Empty = x
fallback (Static x) (Static y) = Static $ Map.union x y
fallback (StaticFiles x) (StaticFiles y) = StaticFiles $ Map.union x y
fallback (Static x) (StaticFiles y) =
    Static $ Map.union x (staticFilesToStatic y)
fallback (StaticFiles x) (Static y) =
    Static $ Map.union (staticFilesToStatic x) y

instance Monoid App where
    mempty = Empty
    mappend = fallback

resolve :: App -> (Request, Body) -> (Response, Body)
resolve Empty _ = emptyResponse status404
resolve (Static m) (request@Request { requestMethod = methodGet }, _)
    | requestMethod request == methodGet =
        Map.findWithDefault (emptyResponse status404) (requestPath request) m
    | otherwise =
        emptyResponse $ if exists then status405 else status404
        where exists = Map.member (requestPath request) m
resolve (StaticFiles mapping) x = resolve static x
    where static = Static $ staticFilesToStatic mapping

-- Any static mappings that can be derived from the app. This represents
-- the subset of the app that could be deployed as files to a reverse proxy
-- in front of the app server (like nginx with the @try_files@ directive).
staticFiles :: App -> StaticFilesMapping
staticFiles Empty = Map.empty
staticFiles (Static mapping) = mapEntriesMaybe f mapping
    where f :: (Path, (Response, Body)) -> Maybe (PathSegments, Body)
          f (path, (response, body))
              | responseStatus response /= status200 = Nothing
              | isJust query = Nothing
              | otherwise = Just (pathSegments :: PathSegments, body)
              where (pathSegments, query) = path
staticFiles (StaticFiles mapping) = mapping
