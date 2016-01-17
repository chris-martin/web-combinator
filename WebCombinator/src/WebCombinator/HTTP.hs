module WebCombinator.HTTP
    ( Method
    , Path
    , Request(..)
    , RequestHeaders
    , Response(..)
    , ResponseHeaders
    , Status
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text
import Network.HTTP.Types
import Prelude (Eq, Show)

-- | The path component of a URL, e.g. @/abc/def%20ghi?x=y@.
type Path = ByteString

data Request = Request
    { requestMethod :: Method
    , requestPath :: Path
    , requestHttpVersion :: HttpVersion
    , requestHeaders :: RequestHeaders
    } deriving (Eq, Show)

data Response = Response
    { requestStatus :: Status
    , responseHttpVersion :: HttpVersion
    , responseHeaders :: ResponseHeaders
    } deriving (Eq, Show)
