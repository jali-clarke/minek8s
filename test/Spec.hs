{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import MineK8s
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), arbitrarySizedNatural)

newtype ArbitraryText = ArbitraryText Text deriving (Show)

newtype ArbitraryVersion = ArbitraryVersion Text deriving (Show)

instance Arbitrary ArbitraryText where
  arbitrary = fmap (ArbitraryText . Text.pack) arbitrary

instance Arbitrary ArbitraryVersion where
  arbitrary =
    let arbitrarySemverSegment = fmap (show :: Int -> String) arbitrarySizedNatural
     in fmap (ArbitraryVersion . Text.pack . concat) $
          sequence
            [ arbitrarySemverSegment,
              pure ".",
              arbitrarySemverSegment,
              pure ".",
              arbitrarySemverSegment
            ]

main :: IO ()
main = hspec $ do
  describe "minecraftInstanceFromCustomResourceAeson" $ do
    minecraftInstanceFromCustomResourceAesonSpec

minecraftInstanceFromCustomResourceAesonSpec :: Spec
minecraftInstanceFromCustomResourceAesonSpec = do
  prop "parses out a MinecraftInstance from a MinecraftInstance crd instance if correctly structured" $
    \(ArbitraryText name, ArbitraryText namespace, ArbitraryVersion version, ArbitraryText nodePortServiceName, nodePortServiceNodePort) ->
      let crdInstance =
            Aeson.object
              [ ("apiVersion", Aeson.String "jali-clarke.ca/v1"),
                ("kind", Aeson.String "MinecraftInstance"),
                ( "metadata",
                  Aeson.object
                    [ ("name", Aeson.String name),
                      ("namespace", Aeson.String namespace)
                    ]
                ),
                ( "spec",
                  Aeson.object
                    [ ("minecraftVersion", Aeson.String version),
                      ( "nodePortService",
                        Aeson.object
                          [ ("serviceName", Aeson.String nodePortServiceName),
                            ("nodePort", Aeson.Number (fromIntegral nodePortServiceNodePort))
                          ]
                      )
                    ]
                )
              ]
          minecraftInstance =
            MinecraftInstance
              { instanceName = name,
                instanceNamespace = namespace,
                minecraftVersion = version,
                nodePortService =
                  NodePortService
                    { serviceName = nodePortServiceName,
                      nodePort = nodePortServiceNodePort
                    }
              }
       in minecraftInstanceFromCustomResourceAeson crdInstance `shouldBe` Right minecraftInstance
  it "fails when apiVersion is incorrect" $
    let crdInstance =
          Aeson.object
            [ ("apiVersion", Aeson.String "egg.mc.muffin/v2"),
              ("kind", Aeson.String "MinecraftInstance"),
              ( "metadata",
                Aeson.object
                  [ ("name", Aeson.String "some-name"),
                    ("namespace", Aeson.String "some-namespace")
                  ]
              ),
              ( "spec",
                Aeson.object
                  [ ("minecraftVersion", Aeson.String "1.2.3"),
                    ( "nodePortService",
                      Aeson.object
                        [ ("serviceName", Aeson.String "some-service-name"),
                          ("nodePort", Aeson.Number 25565)
                        ]
                    )
                  ]
              )
            ]
     in minecraftInstanceFromCustomResourceAeson crdInstance `shouldBe` Left "Error in $: wrong api version: egg.mc.muffin/v2"
  it "fails when kind is incorrect" $
    let crdInstance =
          Aeson.object
            [ ("apiVersion", Aeson.String "jali-clarke.ca/v1"),
              ("kind", Aeson.String "Pod"),
              ( "metadata",
                Aeson.object
                  [ ("name", Aeson.String "some-name"),
                    ("namespace", Aeson.String "some-namespace")
                  ]
              ),
              ( "spec",
                Aeson.object
                  [ ("minecraftVersion", Aeson.String "1.2.3"),
                    ( "nodePortService",
                      Aeson.object
                        [ ("serviceName", Aeson.String "some-service-name"),
                          ("nodePort", Aeson.Number 25565)
                        ]
                    )
                  ]
              )
            ]
     in minecraftInstanceFromCustomResourceAeson crdInstance `shouldBe` Left "Error in $: wrong resource kind: Pod"
  it "fails when version string does not satisfy the expected version string format" $
    let crdInstance =
          Aeson.object
            [ ("apiVersion", Aeson.String "jali-clarke.ca/v1"),
              ("kind", Aeson.String "MinecraftInstance"),
              ( "metadata",
                Aeson.object
                  [ ("name", Aeson.String "some-name"),
                    ("namespace", Aeson.String "some-namespace")
                  ]
              ),
              ( "spec",
                Aeson.object
                  [ ("minecraftVersion", Aeson.String "some-version"),
                    ( "nodePortService",
                      Aeson.object
                        [ ("serviceName", Aeson.String "some-service-name"),
                          ("nodePort", Aeson.Number 25565)
                        ]
                    )
                  ]
              )
            ]
     in minecraftInstanceFromCustomResourceAeson crdInstance `shouldBe` Left "Error in $: invalid minecraft version string: some-version"
