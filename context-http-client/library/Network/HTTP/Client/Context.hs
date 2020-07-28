{-# LANGUAGE NoImplicitPrelude #-}

module Network.HTTP.Client.Context
  ( -- * Updating manager settings
    modifyRequestsWithContext
  , modifyResponsesWithContext
  ) where

import Network.HTTP.Client (BodyReader, ManagerSettings, Request, Response)
import Prelude
import qualified Context
import qualified Network.HTTP.Client as HTTP.Client

-- | Update the provided 'ManagerSettings' to modify all outgoing 'Request'
-- values, utilizing the calling thread's registered context if present.
--
-- Note that the modifying function can be called multiple times, so be sure to
-- define your function to first check if the 'Request' needs modification. See
-- 'HTTP.Client.managerModifyRequest' for details.
--
-- @since 0.1.0.0
modifyRequestsWithContext
  :: Context.Store ctx
  -> (Maybe ctx -> Request -> IO Request)
  -> ManagerSettings
  -> ManagerSettings
modifyRequestsWithContext contextStore updateRequest managerSettings =
  managerSettings
    { HTTP.Client.managerModifyRequest = \initRequest -> do
        -- In case the provided manager settings already had a custom
        -- request-modifying action installed, we make sure to run that on the
        -- request so that we do not miss its modifications.
        request <- originalRequestModifier initRequest

        mContext <- Context.mineMay contextStore
        updateRequest mContext request
    }
  where
    originalRequestModifier = HTTP.Client.managerModifyRequest managerSettings

-- | Update the provided 'ManagerSettings' to modify all incoming 'Response'
-- values, utilizing the calling thread's registered context if present.
--
-- @since 0.1.0.0
modifyResponsesWithContext
  :: Context.Store ctx
  -> (Maybe ctx -> Response BodyReader -> IO (Response BodyReader))
  -> ManagerSettings
  -> ManagerSettings
modifyResponsesWithContext contextStore updateResponse managerSettings =
  managerSettings
    { HTTP.Client.managerModifyResponse = \initResponse -> do
        -- In case the provided manager settings already had a custom
        -- response-modifying action installed, we make sure to run that on the
        -- response so that we do not miss its modifications.
        response <- originalResponseModifier initResponse

        mContext <- Context.mineMay contextStore
        updateResponse mContext response
    }
  where
    originalResponseModifier = HTTP.Client.managerModifyResponse managerSettings
