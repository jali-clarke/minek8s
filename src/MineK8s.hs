{-# LANGUAGE OverloadedStrings #-}

module MineK8s
  ( MinecraftInstance (..),
    NodePortService (..),
    minecraftInstanceFromCustomResourceAeson,
  )
where

import Control.Monad (when)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)

data MinecraftInstance =
  MinecraftInstance
    { instanceName :: Text,
      instanceNamespace :: Text,
      minecraftVersion :: Text,
      nodePortService :: NodePortService
    }
  deriving (Eq, Show)

data NodePortService =
  NodePortService
    { serviceName :: Text,
      nodePort :: Int
    }
  deriving (Eq, Show)

instance Aeson.FromJSON MinecraftInstance where
  parseJSON =
    Aeson.withObject "MinecraftInstance" $ \value -> do
      assertApiVersion value "jali-clarke.ca/v1"
      assertKind value "MinecraftInstance"

      metadata <- value .: "metadata"
      instanceName' <- metadata .: "name"
      instanceNamespace' <- metadata .: "namespace"

      spec <- value .: "spec"
      minecraftVersion' <- spec .: "minecraftVersion"
      nodePortService' <- spec .: "nodePortService"

      pure $ MinecraftInstance instanceName' instanceNamespace' minecraftVersion' nodePortService'

instance Aeson.FromJSON NodePortService where
  parseJSON =
    Aeson.withObject "NodePortService" $ \value ->
      NodePortService <$> value .: "serviceName" <*> value .: "nodePort"

minecraftInstanceFromCustomResourceAeson :: Aeson.Value -> Either String MinecraftInstance
minecraftInstanceFromCustomResourceAeson = Aeson.parseEither Aeson.parseJSON

assertApiVersion :: Aeson.Object -> String -> Aeson.Parser ()
assertApiVersion value expectedApiVersion = do
  apiVersion <- value .: "apiVersion"
  when (apiVersion /= expectedApiVersion) $
    fail ("wrong api version: " <> apiVersion)

assertKind :: Aeson.Object -> String -> Aeson.Parser ()
assertKind value expectedKind = do
  kind <- value .: "kind"
  when (kind /= expectedKind) $
    fail ("wrong resource kind: " <> kind)
