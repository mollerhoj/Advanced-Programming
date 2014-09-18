import Test.HUnit
import CurvySyntax
import CurveAST

test1 = TestCase $ assertBool "Defines x to a moved point" $
  parseString "x= (0,0) -> (1,2)" == Right [Def "x" (Translate (Single (Point (Con 0.0) (Con 0.0))) (Point (Con 1.0) (Con 2.0))) []]

test2 = TestCase $ assertBool "Defines x to be a point" $
  parseString "x=(1,2)" == Right [Def "x" (Single (Point (Con 1.0) (Con 2.0))) []]

test3 = TestCase $ assertBool "Defines x to be a point rotated around the origin" $
  parseString "x=(1,2) rot 3" == Right [Def "x" (Rot (Single (Point (Con 1.0) (Con 2.0))) (Con 3.0)) []]

test4 = TestCase $ assertBool "Defines x and y be z and q" $
  parseString "x=z y=q " == Right [Def "x" (Id "z") [],Def "y" (Id "q") []]

test5 = TestCase $ assertBool "Defines x and line" $
  parseString "x= (0,0) ++ (2,2) " == Right [Def "x" (Connect (Single (Point (Con 0.0) (Con 0.0))) (Single (Point (Con 2.0) (Con 2.0)))) []]

test6 = TestCase $ assertBool "Program can't be empty" $
  parseString "" == Left (SyntaxError "Must start with a definition")

test7 = TestCase $ assertBool "No spaces in names" $
  parseString "x x = 5" == Left (SyntaxError "Must start with a definition")

test8 = TestCase $ assertBool "No 3d" $
  parseString "x=z y=(1,2,3)" == Left (SyntaxError "y=(1,2,3)")

test9 = TestCase $ assertBool "Cant connect to expression" $
  parseString "x=z y=(1,2) ++ (1+1)" ==
  Left (SyntaxError "++ (1+1)")

tests = TestList [ TestLabel "Right" $ TestList [ test1 
                                                , test2
                                                , test3
                                                , test4
                                                , test5
                                                ]
                 , TestLabel "Left"  $ TestList [ test6
                                                , test7
                                                , test8
                                                , test9
                                                ]
                 ]

main = runTestTT tests
