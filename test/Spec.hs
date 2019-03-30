import Parser
import Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main =
  hspec $ do
    describe "arithmeticExpr" $ do
      it "Plus" $
        parse arithmeticExpr "" "+2" `shouldParse` Unary Plus (Constant 2)
      it "Minus" $
        parse arithmeticExpr "" "-2" `shouldParse` Unary Minus (Constant 2)
      it "Addition" $
        parse arithmeticExpr "" "2 + 2" `shouldParse`
        Binary Add (Constant 2) (Constant 2)
      it "Subtraction" $
        parse arithmeticExpr "" "2 - 2" `shouldParse`
        Binary Sub (Constant 2) (Constant 2)
      it "Multiplication" $
        parse arithmeticExpr "" "2 * 2" `shouldParse`
        Binary Mul (Constant 2) (Constant 2)
      it "Division" $
        parse arithmeticExpr "" "2 / 2" `shouldParse`
        Binary Div (Constant 2) (Constant 2)
    describe "assignmentExpr" $ do
      it "Basic" $
        parse assignmentExpr "" "a = 2" `shouldParse`
        Assignment Basic (Identifier "a") (Constant 2)
      it "Addition" $
        parse assignmentExpr "" "a += 2" `shouldParse`
        Assignment Add (Identifier "a") (Constant 2)
      it "Subtraction" $
        parse assignmentExpr "" "a -= 2" `shouldParse`
        Assignment Sub (Identifier "a") (Constant 2)
      it "Multiplication" $
        parse assignmentExpr "" "a *= 2" `shouldParse`
        Assignment Mul (Identifier "a") (Constant 2)
      it "Division" $
        parse assignmentExpr "" "a /= 2" `shouldParse`
        Assignment Div (Identifier "a") (Constant 2)
    describe "comparisonExpr" $ do
      it "Equal" $
        parse comparisonExpr "" "2 == 2" `shouldParse`
        Comparison Equal (Constant 2) (Constant 2)
      it "Not Equal" $
        parse comparisonExpr "" "2 != 2" `shouldParse`
        Comparison NotEqual (Constant 2) (Constant 2)
      it "Less Than" $
        parse comparisonExpr "" "2 < 2" `shouldParse`
        Comparison LessThan (Constant 2) (Constant 2)
      it "Greater Than" $
        parse comparisonExpr "" "2 > 2" `shouldParse`
        Comparison GreaterThan (Constant 2) (Constant 2)
      it "Less Than Or Equal" $
        parse comparisonExpr "" "2 <= 2" `shouldParse`
        Comparison LessEqual (Constant 2) (Constant 2)
      it "Greater Than Or Equal" $
        parse comparisonExpr "" "2 >= 2" `shouldParse`
        Comparison GreaterEqual (Constant 2) (Constant 2)
    describe "constantExpr" $
      it "Return Constant Expression" $
      parse constantExpr "" "2" `shouldParse` Constant 2
    describe "identifierExpr" $
      it "Return Identifier Expression" $
      parse identifierExpr "" "a" `shouldParse` Identifier "a"
    describe "compoundStmt" $
      it "Return Compound Statement" $
      parse compoundStmt "" "{ a = 2; }" `shouldParse`
      CompoundStmt
        [StmtItem (ExprStmt (Assignment Basic (Identifier "a") (Constant 2)))]
    describe "exprStmt" $
      it "Return Expression Statement" $
      parse exprStmt "" "a = 2;" `shouldParse`
      ExprStmt (Assignment Basic (Identifier "a") (Constant 2))
    describe "ifStmt" $
      it "Return If Statement" $
      parse ifStmt "" "if (2) { 2; }" `shouldParse`
      If (Constant 2) (CompoundStmt [StmtItem (ExprStmt (Constant 2))])
    describe "ifElseStmt" $
      it "Return If-Else Statement" $
      parse ifElseStmt "" "if (2) { 2; } else { 2; }" `shouldParse`
      IfElse
        (Constant 2)
        (CompoundStmt [StmtItem (ExprStmt (Constant 2))])
        (CompoundStmt [StmtItem (ExprStmt (Constant 2))])
    describe "whileStmt" $
      it "Return While Statement" $
      parse whileStmt "" "while (2) { 2; }" `shouldParse`
      While (Constant 2) (CompoundStmt [StmtItem (ExprStmt (Constant 2))])
    describe "doWhileStmt" $
      it "Return Do-While Statement" $
      parse doWhileStmt "" "do { 1; } while (1);" `shouldParse`
      DoWhile (CompoundStmt [StmtItem (ExprStmt (Constant 1))]) (Constant 1)
    describe "returnStmt" $
      it "Return 'Return' Statement" $
      parse returnStmt "" "return 0;" `shouldParse` Return (Constant 0)
