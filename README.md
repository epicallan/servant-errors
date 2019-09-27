# servant-errors

[![Hackage](https://img.shields.io/hackage/v/servant-errors.svg?logo=haskell)](https://hackage.haskell.org/package/servant-errors)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/epicallan/servant-errors.svg?logo=travis)](https://travis-ci.org/epicallan/servant-errors)

## Intro

This package implements a wai-middleware targeting servant-server applications with a goal of make it easier to generate custom server error responses.

### Checkout accompanying blogpost for more details.

* [Unifying Servant server error responses](https://lukwagoallan.com/posts/unifying-servant-server-error-responses)

## Motivation

By default, when your servant server application experiences an internal exception during endpoint route resolution, e.g. request body decode errors, the server response is just plain text with a status code in the HTTP headers.

At the same time, if you don't write custom code to customise error responses for errors thrown within servant route handlers; the default response is plain text with an HTTP content-type when provided within `ServerError`.

With `servant-errors`  library, you get a single interface to structure and encode your error responses in one place as `JSON` error response or any other preferred form.

```haskell ignore
-- | A typical servant application is usually of this form

main :: IO ()
main = run 8001 (serve proxyApi handlers)

-- | With 'errorMw' from servant-errors library as an error processing middleware
main :: IO ()
main = run 8001
     $ errorMw @JSON @'["error", "status"]
     -- ^ Structures error response as JSON objects
     -- with 'error' and 'status' strings as error object field keys
     -- note they can be changed to any other preferred strings.
     $ serve proxyApi handlers

-- | The implementation above can also be written as below
-- if you want to output JSON error responses with 'error'
-- and 'status' as the JSON Object keys
main :: IO ()
main = run 8001
     $ errorMwDefJson
     -- ^ Default implementation of structuring error responses as JSON
     -- with no customisation option for JSON error object field keys
     $ serve proxyApi handlers
```



## Complete Usage Example

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Servant.Errors (errorMw, HasErrorBody(..))
import           Servant (ReqBody, (:>), Post, JSON, Accept(..), serve)

-- | A greet message data type for use as Request Body
newtype Greet = Greet { msg :: Text }
  deriving (Generic, Show, FromJSON, ToJSON)

-- servant application
main' :: IO ()
main' = run 8001
  $ errorMw @JSON @'["error", "status"]
  -- ^ @JSON specifies content type encoding of errors
  -- @["error", "status"] specifies error and code text label in resulting JSON error response
  -- when an empty type level list parameter for 'errorMw' is specified
  -- the 'HasErrorBody' instance defaults it to '@["error", "status"]' for JSON and PlainText instances
  -- hence; errorMw @JSON @'[] == @JSON @["error", "status"]
  $ serve api handler
  where
    handler = return . id
    api = Proxy @(ReqBody '[JSON] Greet :> Post '[JSON] Greet)

-- | Example Below shows the derivation of an alternative 'HasErrorBody' instance
-- for JSON error responses if you want to implement more customisation
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | We need a newtype like data type to avoid orphan instances, 'Ctyp' satisfy's that
-- Also note that 'HasErrorBody' instance requires an Accept instance for a content-type

data Ctyp a

{-
 if you are using GHC 8.6 and above you can make use of deriving Via
 for creating the Accept Instance

 >> data Ctyp a
 >>   deriving Accept via JSON
-}

instance Accept (Ctyp JSON) where
  contentType _ = contentType (Proxy @JSON)

instance HasErrorBody (Ctyp JSON) '[] where
  encodeError = undefined -- write your custom implementation

-- | Example Middleware with a different 'HasErrorBody' instance for JSON
errorMwJson :: Application -> Application
errorMwJson =  errorMw @(Ctyp JSON) @'[]
```

If a user submits a wrong request body during an HTTP request the HTTP error response is of the formats below;

Error response body while using this package's error Middleware .
_________________________________________

``` JSON
{
    "status": 400,
    "error": "Error in $: key \"msg\" not present"
}
# The response is JSON encoded and contains an HTTP content-type header plus a status code.
```

Default error response without middleware;
_________________________________________

```
 "Error in $: key \"msg\" not present"

# The response is plain text, contains an HTTP status code but lacks an HTTP content-type header.
```

### Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
