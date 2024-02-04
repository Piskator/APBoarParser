-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Black box tests" [
  
   testsMinimal
  ,testsStringConstDefinitions
  ,testsNumconstDefinition
  ,testsIdentDefinition
  ,testsBoolVal
  ,testsOperRelDiversification
  ,testsOperTypes
  ,testsRelTypes
  ,testsLists

  ]

testsMinimal = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "simple parsing of letter" $
    parseString "'a'" @?=
      Right [SExp (Const (StringVal "a"))],
  testCase "simple parsing of string" $
    parseString "'aaaaa'" @?=
      Right [SExp (Const (StringVal "aaaaa"))]
  ]

testsStringConstDefinitions = testGroup "Tests of StringConst and num and identifer" [  
   testCase "simple parsing of string" $
    parseString "'aaaaa'" @?=
      Right [SExp (Const (StringVal "aaaaa"))]
  ,testCase "simple parsing of escaped \\" $
    parseString "'\\\\'" @?=
      Right [SExp (Const (StringVal "\\"))]
  ,testCase "simple parsing of escaped \'" $
    parseString "'a\\\''" @?=
      Right [SExp (Const (StringVal "a'"))]
  ,testCase "simple parsing of escaped \\n'" $
    parseString "'a\\n'" @?=
      Right [SExp (Const (StringVal "a\n"))]
  ,testCase "simple parsing of 'fo\\\\o\\nb\\na\\'r'" $
    parseString "'fo\\\\\\\\o\\\\b\\na\\'r'" @?=
      Right [SExp (Const (StringVal "fo\\\\o\\b\na\'r"))]
  ,testCase "simple parsing of spaces in '   '" $
    parseString "'   '" @?=
      Right [SExp (Const (StringVal "   "))]
  ,testCase "simple parsing of spaces in ' a b '" $
    parseString "' a b '" @?=
      Right [SExp (Const (StringVal " a b "))]
  ,testCase "simple parsing of spaces before after and in ' a b '" $
    parseString "  ' a b '  " @?=
      Right [SExp (Const (StringVal " a b "))]
  ,testCase "fail simple parsing of unescaped \\" $
    case parseString "'a\''" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ]

testsNumconstDefinition = testGroup "Tests of numConst" [  
   testCase "simple parsing of num" $
    parseString "1020" @?=
      Right [SExp (Const (IntVal 1020))]  
  ,testCase "simple parsing of -0" $
    parseString "-0" @?=
      Right [SExp (Const (IntVal 0))]
  ,testCase "simple parsing of 0" $
    parseString "0" @?=
      Right [SExp (Const (IntVal 0))]
  ,testCase "simple parsing of -12" $
    parseString "-12" @?=
      Right [SExp (Const (IntVal (-12)))]
  ,testCase "simple parsing of -1" $
    parseString "-1" @?=
      Right [SExp (Const (IntVal (-1)))]
  ,testCase "simple parsing of -1" $
    parseString "1" @?=
      Right [SExp (Const (IntVal 1))]
  ,testCase "simple parsing of 10000000000000000000000001" $
    parseString "10000000000000000000000001" @?=
      Right [SExp (Const (IntVal 10000000000000000000000001))]
  ,testCase "simple parsing of spaces before and  after 1040" $
    parseString "       1040   " @?=
      Right [SExp (Const (IntVal 1040))]
  ,testCase "fail simple parsing of spaces between 10  02" $    
    case parseString "10  02" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ,testCase "fail simple parsing of escaped within 10\\02" $    
    case parseString "10\\02" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ,testCase "fail simple parsing of 000" $    
    case parseString "000" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ,testCase "fail simple parsing of 012" $    
    case parseString "012" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ,testCase "fail simple parsing of -012" $    
    case parseString "-012" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p  
  ]

testsIdentDefinition = testGroup "Tests of Ident" [  
   testCase "simple parsing of var1" $
    parseString "var1" @?=
      Right [SExp (Var "var1")]  
  ,testCase "simple parsing of _var" $
    parseString "_var" @?=
      Right [SExp (Var "_var")]  
  ,testCase "simple parsing of _1var" $
    parseString "_1var" @?=
      Right [SExp ((Var "_1var"))]
  ,testCase "simple parsing of Myvar" $
    parseString "Myvar" @?=
      Right [SExp ((Var "Myvar"))]  
  ,testCase "fail simple parsing of 0var" $    
    case parseString "0var" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p  
  ,testCase "fail simple parsing of keyword spacing after: for   " $    
    case parseString "for   " of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ,testCase "fail simple parsing of keyword not" $    
    case parseString "not" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p  
  ]

testsBoolVal = testGroup "Tests of Value" [  
   testCase "simple parsing of True" $
    parseString "   True" @?=
      Right [SExp (Const TrueVal)]
  ,testCase "simple parsing of None" $
    parseString "   None   " @?=
      Right [SExp (Const NoneVal)]
  ,testCase "simple parsing of False" $
    parseString "   False   " @?=
      Right [SExp (Const FalseVal)]
  ,testCase "simple parsing of f ( ) " $
    parseString "f ( ) " @?=
      Right [SExp (Call "f" [])]
  ,testCase "simple parsing of F()" $
    parseString "F()" @?=
      Right [SExp (Call "F" [])]
  ,testCase "simple parsing of g(True)" $
    parseString "g(True)" @?=
      Right [SExp (Call "g" [Const TrueVal])]
  ,testCase "simple parsing of f ( g(True) )" $
    parseString "f ( g( True ) )" @?=
      Right [SExp (Call "f" [Call "g" [Const TrueVal]])]
  ,testCase "simple parsing of Var1" $
    parseString "Var1" @?=
      Right [SExp (Var "Var1")]
  ,testCase "simple parsing of (True)" $
    parseString "(True)" @?=
      Right [SExp (Const TrueVal)]
  ,testCase "simple parsing of ()" $
    case parseString "()" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p 
  ,testCase "simple parsing of: x not in y" $
    parseString "x not in y" @?=
      Right [SExp (Not (Oper In (Var "x") (Var "y")))]
  ,testCase "simple parsing of: [x!=y,x>=y,x<=y,x not in y]" $
    parseString "[x!=y,x>=y,x<=y,x not in y]" @?=
      Right [SExp (List [Not (Oper Eq (Var "x") (Var "y")),Not (Oper Less (Var "x") (Var "y")),Not (Oper Greater (Var "x") (Var "y")),Not (Oper In (Var "x") (Var "y"))])]
  ,testCase "simple parsing of x%y-z>(not u)" $
    parseString "x%y-z>(not u)" @?=
      Right [SExp (Oper Greater (Oper Minus (Oper Mod (Var "x") (Var "y")) (Var "z")) (Not (Var "u")))]
  ,testCase "not needed: [(x)not\tin(not(y)),[(x)for\ty\tin[z]if(u)]]" $
    parseString "[(x)not\tin(not(y)),[(x)for\ty\tin[z]if(u)]]" @?=
      Right [SExp (List [Not (Oper In (Var "x") (Not (Var "y"))),Compr (Var "x") [CCFor "y" (List [Var "z"]),CCIf (Var "u")]])]
  ,testCase "parse 'a\\\n b\\n\\\nc\\\n\\nd'" $
    parseString "'a\\\n b\\n\\\nc\\\n\\nd'" @?=
      Right [SExp (Const (StringVal "a b\nc\nd"))]
  ,testCase "[x if y]" $
    case parseString "[x if y]" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p 
  ,testCase "[x if y for u in v]" $
    case parseString "[x if y for u in v]" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p 
  ]

testsOperRelDiversification = testGroup "Tests divisification of operations" [  
   testCase "parsing of 2*2" $
    parseString "2*2" @?=
      Right [SExp (Oper Times (Const (IntVal 2)) (Const (IntVal 2)))]  
  ,testCase "parsing of 2 == 2*2" $
    parseString "2 == 2*2" @?=
      Right [SExp (Oper Eq (Const (IntVal 2)) (Oper Times (Const (IntVal 2)) (Const (IntVal 2))))] 
  ,testCase "parsing of 2 == 2*2 + 4" $
    parseString "2 == 2*2 + 4 " @?=
      Right [SExp (Oper Eq (Const (IntVal 2)) (Oper Plus (Oper Times (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 4))))]
  ,testCase "parsing of 2 == 2 + 4" $
    parseString " 2 == 2 + 4 " @?=
      Right [SExp (Oper Eq (Const (IntVal 2)) (Oper Plus (Const (IntVal 2)) (Const (IntVal 4))))]
  ,testCase "parsing of 2 + 4 == 2 + 4" $
    parseString " 2 + 4 == 2 + 4 " @?=
      Right [SExp (Oper Eq (Oper Plus (Const (IntVal 2)) (Const (IntVal 4))) (Oper Plus (Const (IntVal 2)) (Const (IntVal 4))))]
  ,testCase "parsing of 2*2 + 3 * 4" $
    parseString "2*2 + 3 * 4" @?=
      Right [SExp (Oper Plus (Oper Times (Const (IntVal 2)) (Const (IntVal 2))) (Oper Times (Const (IntVal 3)) (Const (IntVal 4))))]
  ,testCase "parsing of 2*3//4%5 + 4 + 5" $
    parseString "2*3//4%5 + 4 + 5" @?=
      Right [SExp (Oper Plus (Oper Plus (Oper Mod (Oper Div (Oper Times (Const (IntVal 2)) (Const (IntVal 3))) (Const (IntVal 4))) (Const (IntVal 5))) (Const 
            (IntVal 4))) (Const (IntVal 5)))]
  ]

testsOperTypes = testGroup "Tests types of operations" [  
   testCase "parsing of 2//2" $
    parseString "2 // 2" @?=
      Right [SExp (Oper Div (Const (IntVal 2)) (Const (IntVal 2)))]  
  ,testCase "parsing of 2 == 2*2" $
    parseString "2 == 2 // 2" @?=
      Right [SExp (Oper Eq (Const (IntVal 2)) (Oper Div (Const (IntVal 2)) (Const (IntVal 2))))] 
  ,testCase "parsing of 2 == 2%2 + 4" $
    parseString "2 == 2%2 + 4 " @?=
      Right [SExp (Oper Eq (Const (IntVal 2)) (Oper Plus (Oper Mod (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 4))))]
  ,testCase "parsing of 2 == 2 + 4" $
    parseString " 2 == 2 + 4 " @?=
      Right [SExp (Oper Eq (Const (IntVal 2)) (Oper Plus (Const (IntVal 2)) (Const (IntVal 4))))]
  ,testCase "parsing of 2 - 4 == 2 - 4" $
    parseString " 2 - 4 == 2 - 4 " @?=
      Right [SExp (Oper Eq (Oper Minus (Const (IntVal 2)) (Const (IntVal 4))) (Oper Minus (Const (IntVal 2)) (Const (IntVal 4))))]
  ,testCase "parsing of 2%2 + 3 * 4" $
    parseString "2*2 + 3 * 4" @?=
      Right [SExp (Oper Plus (Oper Times (Const (IntVal 2)) (Const (IntVal 2))) (Oper Times (Const (IntVal 3)) (Const (IntVal 4))))]  
  ,testCase "parsing of 2*3//4%5" $
    parseString "2*3//4%5" @?=
      Right [SExp (Oper Mod (Oper Div (Oper Times (Const (IntVal 2)) (Const (IntVal 3))) (Const (IntVal 4))) (Const (IntVal 5)))]
  ]


testsRelTypes = testGroup "Tests types of operations" [  
   testCase "parsing of 2 < 4" $
    parseString "2 < 4" @?=
      Right [SExp (Oper Less (Const (IntVal 2)) (Const (IntVal 4)))]
  ,testCase "parsing of 2 > 2 // 2" $
    parseString "2 > 2 // 2" @?=
      Right [SExp (Oper Greater (Const (IntVal 2)) (Oper Div (Const (IntVal 2)) (Const (IntVal 2))))]
  ,testCase "parsing of 2 >= 2%2 + 4" $
    parseString "2 >= 2%2 + 4 " @?=
      Right [SExp (Not (Oper Less (Const (IntVal 2)) (Oper Plus (Oper Mod (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 4)))))]
  ,testCase "parsing of 2 =< 2 + 4" $
    parseString " 2 <= 2 + 4 " @?=
      Right [SExp (Not (Oper Greater (Const (IntVal 2)) (Oper Plus (Const (IntVal 2)) (Const (IntVal 4)))))]
  ,testCase "not -4 == 2 - 4" $
    parseString " not -4 == 2 - 4 " @?=
      Right [SExp (Not (Oper Eq (Const (IntVal (-4))) (Oper Minus (Const (IntVal 2)) (Const (IntVal 4)))))]
  ,testCase "not 2 - 4" $
    parseString "not 2 - 4" @?=
      Right [SExp (Not (Oper Minus (Const (IntVal 2)) (Const (IntVal 4))))]
  ,testCase "not True" $
    parseString "not True" @?=
      Right [SExp (Not (Const TrueVal))]
  ,testCase "notX" $
    parseString "notX" @?=
      Right [SExp (Not (Const TrueVal))]
  ,testCase "not4" $
    parseString "not4" @?=
      Right [SExp (Not (Const TrueVal))]
  ,testCase "not not not True" $
    parseString "not not not True" @?=
      Right [SExp $ Not $ Not (Not (Const TrueVal))]
  

  ,testCase "fail simple parsing of 2 < 4 < 6" $    
    case parseString "2 < 4 < 6" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p 
  
  ,testCase "fail simple parsing of 2 == 4 == 6" $    
    case parseString "2 == 4 == 6" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p 
  ]



testsLists = testGroup "Tests " [  
   testCase "parsing of []" $
    parseString "[]" @?=
      Right [SExp (List [])]
  ,
  testCase "parsing of [4,3]" $
    parseString "[4,3]" @?=
      Right [SExp (List [Const (IntVal 4),Const (IntVal 3)])]
  ,
  testCase "parsing of [4,'howdy']" $
    parseString "[4,            'howdy'          ]" @?=
      Right [SExp (List [Const (IntVal 4),Const (StringVal "howdy")])]
  ,testCase "parsing of [4,5, 6, 7, 9]" $
    parseString "[4,5, 6, 7, 9]" @?=
      Right [SExp (List [Const (IntVal 4),Const (IntVal 5),Const (IntVal 6),Const (IntVal 7),Const (IntVal 9)])]
  ,testCase "parsing of [[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]" $
    parseString "[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]" @?=
      Right [SExp (List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List [List []]]]]]]]]]]]]]]]]]]])]

  ]
