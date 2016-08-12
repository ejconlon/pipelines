{-# LANGUAGE OverloadedStrings #-}

module Pipelines.Command where

import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import qualified Data.Text        as T

-- | A stringy identifier for a Task's action in the world
type Action = T.Text

-- | The Result of running a Task
-- TODO(eric) possibly a retry state
data Result = OkResult | FailResult deriving (Show, Eq)

instance A.FromJSON Result where
  parseJSON (A.String t) =
    case t of
      "ok" -> return OkResult
      "fail" -> return FailResult
      _ -> fail ("invalid Result " ++ T.unpack t)
  parseJSON invalid = A.typeMismatch "Result" invalid

instance A.ToJSON Result where
  toJSON OkResult = A.String "ok"
  toJSON FailResult = A.String "fail"

class Monad b => MonadCommand b where
  command :: Action -> b Result
