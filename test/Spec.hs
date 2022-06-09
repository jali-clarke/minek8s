import Hello
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "should be \"hello world!\"" $ helloString `shouldBe` "hello world!"
