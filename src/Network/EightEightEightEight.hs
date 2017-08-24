{-# LANGUAGE OverloadedStrings #-}

module Network.EightEightEightEight (
    connect
  , query
  -- * types
  , EightEightEightEight
  , Eight888Error
  , Name
  , RRType(..)
  , EDNSSubnet(..)
  , IPText
  , Mask
  , Reply(..)
  , Question(..)
  , Record(..)
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.Text as Text
import           Network.HTTP2
import           Network.HTTP2.Client
import           Network.HTTP2.Client.Helpers
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

data Question = Question {
    questionName :: Name
  , questionType :: Int
  } deriving (Eq, Ord, Show)

instance FromJSON Question where
    parseJSON = withObject "Question" $ \v ->
        Question <$> v .: "name"
                 <*> v .: "type"

data Record = Record {
    recordName :: Name
  , recordType :: Int
  , recordTTL  :: Int
  , recordData :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON Record where
    parseJSON = withObject "Record" $ \v ->
        Record <$> v .: "name"
               <*> v .: "type"
               <*> v .: "TTL"
               <*> v .: "data"

data Reply = Reply {
    replyStatus :: Int
  , replyTC :: Bool -- ^ truncated
  , replyRD :: Bool
  , replyRA :: Bool
  , replyAD :: Bool
  , replyCD :: Bool
  , replyQuestions :: [Question]
  , replyAnswers   :: [Record]
  , replyComment   :: Maybe Text
  , replyAuthority :: Maybe [Record]
  } deriving (Eq, Ord, Show)

instance FromJSON Reply where
    parseJSON = withObject "Reply" $ \v ->
        Reply <$> v .: "Status"
              <*> v .: "TC"
              <*> v .: "RD"
              <*> v .: "RA"
              <*> v .: "AD"
              <*> v .: "CD"
              <*> v .: "Question"
              <*> v .: "Answer"
              <*> v .:? "Comment"
              <*> v .:? "Authority"

type QueryParams = Text

type Name = Text

type IPText = Text

type Mask = Int

data EDNSSubnet = EDNSSubnet !IPText !Mask

data RRType = A | AAAA | CNAME | MX | ANY | Specified Text

queryParams :: RRType -> Maybe EDNSSubnet -> Name -> QueryParams
queryParams rr (Just edns) name = Text.intercalate "&"
  [ "name=" <> name
  , "type=" <> rrtoType rr
  , "_edns_client_subnet=" <> ednsToText edns
  ]
queryParams rr Nothing name = Text.intercalate "&"
  [ "name=" <> name
  , "type=" <> rrtoType rr
  ]

ednsToText :: EDNSSubnet -> Text
ednsToText (EDNSSubnet ipTxt mask) =
    ipTxt <> "/" <> (convertString $ show mask)

rrtoType :: RRType -> Text
rrtoType A = "A"
rrtoType AAAA = "AAAA"
rrtoType CNAME = "CNAME"
rrtoType MX = "MX"
rrtoType ANY = "ANY"
rrtoType (Specified t) = t

queryPath :: RRType -> Maybe EDNSSubnet -> Name -> QueryParams
queryPath rr edns name = "/resolve?" <> queryParams rr edns name

type Eight888Error = Text

data EightEightEightEight = EightEightEightEight {
    query :: RRType -> Maybe EDNSSubnet -> Name -> IO (Either Eight888Error Reply)
  }

connect :: TLS.ClientParams -> IO EightEightEightEight
connect tls = do
    conn <- newHttp2Client "dns.google.com" 443 4096 4096 tls []
    _addCredit (_incomingFlowControl conn) 10000000 -- 10Mio
    let query :: RRType -> Maybe EDNSSubnet -> Name -> IO (Either Eight888Error Reply)
        query rr edns name = do
            let path = convertString $ queryPath rr edns name
            print path
            let hdrs = [ (":method", "GET")
                       , (":scheme", "https")
                       , (":authority", "dns.google.com")
                       , (":path", path)
                       ]
            val <- withHttp2Stream conn $ \stream ->
                StreamDefinition (headers stream hdrs setEndStream)
                                 (\fcIN fcOUT -> fromStreamResult <$> waitStream stream fcIN)
            _ <- _updateWindow $ _incomingFlowControl conn
            return $ case val of
                Left _  ->
                    Left "too much concurrency"
                Right (Left err) -> 
                    Left $ convertString $ show err
                Right (Right (status,dat)) ->  
                    let rec = eitherDecode (convertString dat) :: Either String Reply
                    in either (Left . convertString) Right rec

    return $ EightEightEightEight query
