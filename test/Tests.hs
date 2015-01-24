module Tests(tests) where

import Distribution.TestSuite
import KMC.Syntax.External
import KMC.Syntax.Parser
import KMC.Syntax.Config

tests :: IO [Test]
tests = return parserTests

testInstance :: String -> IO Progress -> TestInstance
testInstance n prog = let inst = TestInstance
                                 { name =  n
                                 , tags = []
                                 , options = []
                                 , setOption = \_ _ -> Right inst
                                 , run = prog
                                 }
                      in inst

parserTests :: [Test]
parserTests = map (Test . makeTest) parseUnparseTests
    where
      makeTest (Left (str, shouldSucceed)) =
        testInstance ("Parse/Unparse: " ++ str) (parseUnparse str shouldSucceed)
      makeTest (Right re) = error "TODO: Make test case"

      parseUnparse str shouldSucceed =
        return $
        case parseRegex basicRegexParser str of
          Left err -> if shouldSucceed then
                          Finished (Fail err)
                      else
                          Finished Pass
          Right (_, re) -> if not shouldSucceed then
                             Finished (Fail "Expected failure. Parse suceeded.")
                           else
                             if unparse re == str then
                               Finished Pass
                             else
                               Finished (Fail $ "Gave " ++ str ++ ". Parsed: " ++ unparse re)


{----------------------------------------------------------------------------------------}

parseUnparseTests :: [Either (String, Bool) Regex]
parseUnparseTests =
  [Left ("a*x", True)
  ,Left ("a{1,5}y", True)
  ,Left ("a+?", True)
  ,Left ("", False)]
