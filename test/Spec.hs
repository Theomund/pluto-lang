import           Parser
import           Syntax
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

main :: IO ()
main =
  hspec $ do
    describe "Parser" $ do
      it "constantExpr" $
        parse constantExpr "" "2" `shouldParse` (Constant 2)
      it "identifierExpr" $
        parse identifierExpr "" "a" `shouldParse` (Identifier "a")
      it "callExpr" $
        parse callExpr "" "a()" `shouldParse` (Call "a" [])
