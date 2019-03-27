import Parser
import Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main =
  hspec $ do
    describe "arithmeticExpression" $ do
      it "Plus" $
        parse arithmeticExpression "" "+2" `shouldParse` Unary Plus (Constant 2)
      it "Minus" $
        parse arithmeticExpression "" "-2" `shouldParse`
        Unary Minus (Constant 2)
      it "Addition" $
        parse arithmeticExpression "" "2 + 2" `shouldParse`
        Binary Add (Constant 2) (Constant 2)
      it "Subtraction" $
        parse arithmeticExpression "" "2 - 2" `shouldParse`
        Binary Sub (Constant 2) (Constant 2)
      it "Multiplication" $
        parse arithmeticExpression "" "2 * 2" `shouldParse`
        Binary Mul (Constant 2) (Constant 2)
      it "Division" $
        parse arithmeticExpression "" "2 / 2" `shouldParse`
        Binary Div (Constant 2) (Constant 2)
    describe "assignmentExpression" $ do
      it "Basic" $
        parse assignmentExpression "" "a = 2" `shouldParse`
        Assignment Basic (Variable "a") (Constant 2)
      it "Addition" $
        parse assignmentExpression "" "a += 2" `shouldParse`
        Assignment Add (Variable "a") (Constant 2)
      it "Subtraction" $
        parse assignmentExpression "" "a -= 2" `shouldParse`
        Assignment Sub (Variable "a") (Constant 2)
      it "Multiplication" $
        parse assignmentExpression "" "a *= 2" `shouldParse`
        Assignment Mul (Variable "a") (Constant 2)
      it "Division" $
        parse assignmentExpression "" "a /= 2" `shouldParse`
        Assignment Div (Variable "a") (Constant 2)
    describe "comparisonExpression" $ do
      it "Equal" $
        parse comparisonExpression "" "2 == 2" `shouldParse`
        Comparison Equal (Constant 2) (Constant 2)
      it "Not Equal" $
        parse comparisonExpression "" "2 != 2" `shouldParse`
        Comparison NotEqual (Constant 2) (Constant 2)
      it "Less Than" $
        parse comparisonExpression "" "2 < 2" `shouldParse`
        Comparison LessThan (Constant 2) (Constant 2)
      it "Greater Than" $
        parse comparisonExpression "" "2 > 2" `shouldParse`
        Comparison GreaterThan (Constant 2) (Constant 2)
      it "Less Than Or Equal To" $
        parse comparisonExpression "" "2 <= 2" `shouldParse`
        Comparison LessEqual (Constant 2) (Constant 2)
      it "Greater Than Or Equal To" $
        parse comparisonExpression "" "2 >= 2" `shouldParse`
        Comparison GreaterEqual (Constant 2) (Constant 2)
    describe "constantExpression" $
      it "Return Constant" $
      parse constantExpression "" "2" `shouldParse` Constant 2
    describe "variableExpression" $
      it "Return Variable" $
      parse variableExpression "" "a" `shouldParse` Variable "a"
