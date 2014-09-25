import Test.QuickCheck as QC
import CurvySyntax

-- Helpers
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

-- Properties
prop_leading_spaces p = isRight $ parseString p

main = quickCheck prop_leading_spaces

validPrograms = QC.elements (['a'..'z'])

newtype ValidPrograms = VP String deriving (Show,Eq)

instance QC.Arbitrary ValidPrograms where
  arbitrary = fmap VP $ do n <- choose (10,5)
                           vectorOf n validPrograms
                     
