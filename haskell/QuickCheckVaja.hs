import Test.QuickCheck

prop :: [Int] -> [Int] -> Bool
prop xs ys = (length (xs ++ ys) == length xs + length xs)

main = do quickCheck (prop)