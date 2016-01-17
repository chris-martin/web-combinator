{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import qualified Data.Map as Map
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Prelude (($), (==), IO, Maybe(..), Show, map, return)
import Test.Hspec
import Test.QuickCheck
import WebCombinator.Message
import WebCombinator.Pure

methods :: [Method]
methods = [ methodGet
          , methodPost
          , methodHead
          , methodPut
          , methodDelete
          , methodTrace
          , methodConnect
          , methodOptions
          , methodPatch]

methodGen :: Gen Method
methodGen = oneof $ map return methods

requestGen :: Gen Request
requestGen = do
    method <- methodGen
    return Request { requestMethod = methodGet
                   , requestPath = (["/"], Nothing)
                   , requestHeaders = [] }

newtype Req = Req (Request, Body) deriving (Show)

instance Arbitrary Req where
    arbitrary = do
        r <- requestGen
        return $ Req (r, emptyBody)

main :: IO ()
main = hspec $ do
  describe "The empty webapp" $ do
    it "responds to everything with 404" $ do
      property $ \(Req r) -> resolve Empty r == emptyResponse status404

    it "has no static files" $
      staticFiles Empty `shouldBe` Map.empty
