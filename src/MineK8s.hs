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
      apiVersion <- value .: "apiVersion"
      when (apiVersion /= "jali-clarke.ca/v1") $
        fail ("wrong api version: " <> apiVersion)
      kind <- value .: "kind"
      when (kind /= "MinecraftInstance") $
        fail ("wrong resource kind: " <> kind)
      metadataAeson <- value .: "metadata"
      specAeson <- value .: "spec"
      flip (Aeson.withObject "MinecraftInstance.metadata") metadataAeson $ \metadata -> do
        instanceName' <- metadata .: "name"
        instanceNamespace' <- metadata .: "namespace"
        flip (Aeson.withObject "MinecraftInstance.spec") specAeson $ \spec -> do
          minecraftVersion' <- spec .: "minecraftVersion"
          nodePortServiceAeson <- spec .: "nodePortService"
          nodePortService' <- Aeson.parseJSON nodePortServiceAeson
          pure $ MinecraftInstance instanceName' instanceNamespace' minecraftVersion' nodePortService'

instance Aeson.FromJSON NodePortService where
  parseJSON =
    Aeson.withObject "NodePortService" $ \value ->
      NodePortService <$> value .: "serviceName" <*> value .: "nodePort"

minecraftInstanceFromCustomResourceAeson :: Aeson.Value -> Either String MinecraftInstance
minecraftInstanceFromCustomResourceAeson = Aeson.parseEither Aeson.parseJSON
