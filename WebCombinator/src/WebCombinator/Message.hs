module WebCombinator.Message
    ( Method
    , Path
    , PathSegments
    , Request(..)
    , Response(..)
    , Status
    , decodePath
    ) where

import qualified Data.ByteString as ByteString
import Data.Text
import Network.HTTP.Types (Query, decodePathSegments, parseQuery)
import Prelude (($), (==), Bool, Eq, Maybe(..), Show)
import qualified WebCombinator.HTTP as HTTP
import WebCombinator.HTTP (Method, RequestHeaders, ResponseHeaders, Status)

type PathSegments = [Text]
-- | The query string is represented by 'Maybe' to distinguish, e.g.,
-- | @/foo@ from @/foo?@.
type Path = (PathSegments, Maybe Query)

decodePath :: HTTP.Path -> Path
decodePath b =
    let (x, y) = ByteString.break (== 63) b -- split on question mark
        noQ = same ByteString.length x b    -- if x == b, there is no query
        query = if noQ then Nothing else Just $ parseQuery y
    in (decodePathSegments x, query)

same :: (Eq b) => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

data Request = Request
    { requestMethod :: Method
    , requestPath :: Path
    , requestHeaders :: RequestHeaders
    } deriving (Eq, Show)

data Response = Response
    { responseStatus :: Status
    , responseHeaders :: ResponseHeaders
    } deriving (Eq, Show)
