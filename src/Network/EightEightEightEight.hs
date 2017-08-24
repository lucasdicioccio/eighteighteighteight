{-# LANGUAGE OverloadedStrings #-}

module Network.EightEightEightEight where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.Text
import           Network.HTTP2
import           Network.HTTP2.Client
import           Network.HTTP2.Client.Helpers
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

type QueryParams = Text
type Name = Text
type IPText = Text
type Mask = Int
data EDNSSubnet = EDNSSubnet !IPText !Mask

data RRType = A | AAAA | CNAME | MX | ANY | Specified Text

queryParams :: RRType -> Maybe EDNSSubnet -> Name -> QueryParams
queryParams rr edns name = "name=" <> name

queryPath :: RRType -> Maybe EDNSSubnet -> Name -> QueryParams
queryPath rr edns name = "/resolve?" <> queryParams rr edns name

type Eight888Error = Text

data EightEightEightEight = EightEightEightEight {
    query :: RRType -> Maybe EDNSSubnet -> Name -> IO (Either Eight888Error StreamResponse)
  }

tls :: TLS.ClientParams
tls = TLS.ClientParams {
      TLS.clientWantSessionResume    = Nothing
    , TLS.clientUseMaxFragmentLength = Nothing
    , TLS.clientServerIdentification = ("127.0.0.1", "")
    , TLS.clientUseServerNameIndication = True
    , TLS.clientShared               = def
    , TLS.clientHooks                = def { TLS.onServerCertificate = \_ _ _ _ -> return []
                                           }
    , TLS.clientSupported            = def { TLS.supportedCiphers = TLS.ciphersuite_default }
    , TLS.clientDebug                = def
    }

connect :: IO EightEightEightEight
connect = do
    conn <- newHttp2Client "dns.google.com" 443 4096 4096 tls []
    _addCredit (_incomingFlowControl conn) 10000000 -- 10Mio
    _ <- async $ forever $ do
            updated <- _updateWindow $ _incomingFlowControl conn
            threadDelay 100000 -- 100ms
    let query :: RRType -> Maybe EDNSSubnet -> Name -> IO (Either Eight888Error StreamResponse)
        query rr edns name = do
            let hdrs = [ (":method", "GET")
                       , (":scheme", "https")
                       , (":authority", "dns.google.com")
                       , (":path", convertString $ queryPath rr edns name)
                       ]
            val <- withHttp2Stream conn $ \stream ->
                StreamDefinition (headers stream hdrs setEndStream)
                                 (\fcIN fcOUT -> fromStreamResult <$> waitStream stream fcIN)
            return $ case val of
                Left _  ->
                    Left "too much concurrency"
                Right (Left err) -> 
                    Left $ convertString $ show err
                Right (Right x) -> 
                      (Right x)

    return $ EightEightEightEight query

defaultMain :: IO ()
defaultMain = do
    eight888 <- connect
    query eight888 A Nothing "haskell.org" >>= print
    query eight888 A Nothing "cloud.google.com" >>= print
