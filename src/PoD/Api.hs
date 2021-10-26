{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module PoD.Api (podTradeSession, withSessionSearch, doSearch) where

import Data.Aeson.Types (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Lens.Micro.Platform ((&), (.~), (^.))
import qualified Network.HTTP.Client.Internal as HC
import Network.URI.Encode (decodeByteString, decodeText)
import Network.Wreq
  ( asJSON,
    cookies,
    defaults,
    header,
    responseBody,
  )
import qualified Network.Wreq.Session as S
import PoD.Parser (parsePodUri)
import PoD.Types (Hit (..), SearchQuery (SearchQuery), SearchResponse (SearchResponse))

podTradeSession :: IO S.Session
podTradeSession = do
  sess <- S.newSession
  S.get sess "https://beta.pathofdiablo.com/trade-search"
  return sess

doSearch :: SearchQuery -> IO SearchResponse
doSearch q = do
  sess <- podTradeSession
  withSessionSearch sess q

withSessionSearch :: S.Session -> SearchQuery -> IO SearchResponse
withSessionSearch sess q = do
  cookieJar <- S.getSessionCookieJar sess
  res <- asJSON =<< S.postWith (opts cookieJar) sess "https://beta.pathofdiablo.com/api/v2/trade/search" (toJSON q)
  return (res ^. responseBody)
  where
    toCookieList = maybe [] HC.expose
    getXsrfToken cookies = head $ HC.cookie_value <$> filter (\c -> "XSRF-TOKEN" == HC.cookie_name c) (toCookieList cookies)
    opts cookieJar =
      defaults
        & header "authority" .~ ["beta.pathofdiablo.com"]
        & header "sec-ch-ua" .~ ["\"Google Chrome\";v=\"89\", \"Chromium\";v=\"89\", \";Not A Brand\";v=\"99\""]
        & header "accept" .~ ["application/json, text/plain, */*"]
        & header "x-xsrf-token" .~ [decodeByteString . getXsrfToken $ cookieJar]
        & header "sec-ch-ua-mobile" .~ ["?0"]
        & header "user-agent" .~ ["Mozilla/5.0 (Macintosh; Intel Mac OS X 11_2_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36"]
        & header "content-type" .~ ["application/json;charset=UTF-8"]
        & header "origin" .~ ["https://beta.pathofdiablo.com"]
        & header "sec-fetch-site" .~ ["same-origin"]
        & header "sec-fetch-mode" .~ ["cors"]
        & header "sec-fetch-dest" .~ ["empty"]
        -- & header "referer" .~ [encodeUtf8 t]
        & header "accept-language" .~ ["en-GB,en;q=0.9"]
        & cookies .~ cookieJar
