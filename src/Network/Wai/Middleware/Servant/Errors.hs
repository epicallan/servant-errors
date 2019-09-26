{-
  A Wai middleware that uniformly structures errors with in a servant application.
  The library assumes all HTTP responses with status code greater than 200 and
  without an HTTP content type are error responses. This assumption is derived
  from servant server error handling implementation.
-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Network.Wai.Middleware.Servant.Errors
  ( getConfig
  , errorMw
  , errorMwDefJson
  , HasErrorBody (..)
  )where

import Data.Aeson (Value (..), encode)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (Header, Status (..), hContentType)
import Network.Wai (Application, Response, responseHeaders, responseLBS, responseStatus,
                    responseToStream)
import Servant.API.ContentTypes (Accept (..), JSON, PlainText)

newtype StatusCode = StatusCode { unStatusCode :: Int }
  deriving (Eq, Ord, Show)

newtype ErrorMsg = ErrorMsg { unErrorMsg :: T.Text }
  deriving Show

data ErrorConfig = ErrorConfig
  { errName       :: T.Text
  , errStatusName :: T.Text
  } deriving Show

-- 'opts' is used to set custom error and status code labels
-- when left as an Empty type level list, it default's to ["error", "status"]
-- in JSON and PlainText HasErrorBody instances
class Accept ctyp => HasErrorBody ctyp (opts :: [Symbol]) where
  encodeError :: StatusCode -> ErrorMsg -> LB.ByteString

instance  (KnownSymbol errLabel, KnownSymbol statusLabel)
  => HasErrorBody JSON '[errLabel, statusLabel] where
    encodeError = encodeAsJsonError (getConfig @statusLabel @errLabel)

instance HasErrorBody JSON '[] where
  encodeError = encodeError @JSON @["error", "status"]

instance  (KnownSymbol errLabel, KnownSymbol statusLabel)
  => HasErrorBody PlainText '[errLabel, statusLabel] where
    encodeError = encodeAsPlainText (getConfig @statusLabel @errLabel)

instance HasErrorBody PlainText '[] where
  encodeError = encodeError @JSON @["error", "status"]

getConfig
  :: forall errLabel statusLabel .(KnownSymbol errLabel, KnownSymbol statusLabel)
  => ErrorConfig
getConfig = ErrorConfig (label (Proxy @errLabel)) (label (Proxy @statusLabel))
  where
    label :: KnownSymbol t => Proxy t -> T.Text
    label proxy = cs $ symbolVal proxy

encodeAsJsonError :: ErrorConfig -> StatusCode -> ErrorMsg -> LB.ByteString
encodeAsJsonError ErrorConfig {..} code content =
  encode $ Object
         $ H.fromList
           [ (errName, String $ unErrorMsg content)
           , (errStatusName, Number $ toScientific code )
           ]
   where
     toScientific :: StatusCode -> Scientific
     toScientific = fromInteger . fromIntegral @_ @Integer . unStatusCode

encodeAsPlainText :: ErrorConfig -> StatusCode -> ErrorMsg -> LB.ByteString
encodeAsPlainText ErrorConfig {..} code content =
  cs $  errName
     <> unErrorMsg content
     <> errStatusName
     <> cs (show $ unStatusCode code)

errorMwDefJson :: Application -> Application
errorMwDefJson = errorMw @JSON @'[]

errorMw :: forall ctyp opts. HasErrorBody ctyp opts => Application -> Application
errorMw baseApp req respond =
  baseApp req $ \ response -> do
     let status      = responseStatus response
         mcontentType = getContentTypeHeader response
     case (status, mcontentType) of
       (Status 200 _, _)                     -> respond response
       (Status code _, Nothing) | code > 200 ->
         newResponse @ctyp @opts status response >>= respond
       _                                     -> respond response

getContentTypeHeader :: Response -> Maybe Header
getContentTypeHeader = find ((hContentType ==) . fst) . responseHeaders

-- | In the event that an error has an empty body
-- such as 404 errors, we use error status message for error body
newResponse
  :: forall ctyp opts . HasErrorBody ctyp opts
  => Status
  -> Response
  -> IO Response
newResponse status@(Status code statusMsg) response = do
  body <- responseBody response
  let header = (hContentType,  M.renderHeader $ contentType (Proxy @JSON) )
      content = ErrorMsg . cs $ if body == mempty then statusMsg else body
      newContent = encodeError @ctyp @opts (StatusCode code) content
  return $ responseLBS status [header] newContent

responseBody :: Response -> IO B.ByteString
responseBody res =
  let (_status, _headers, streamBody) = responseToStream res in
  streamBody $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    cs . toLazyByteString <$> readIORef content
