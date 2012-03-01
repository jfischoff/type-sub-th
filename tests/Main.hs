module Main where
import Language.Haskell.TH
import Language.Haskell.TH.TypeSub
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Language.Haskell.TH.Instances
import Test.QuickCheck.Checkers
import Data.List
import Data.Generics.Uniplate.Data
    
main = defaultMainWithArgs tests ["-a 100", "-o 4"]

--I need to test each function with every possible constructor type
--I should test that any type that is one I handle, does not cause a crash

tests = [
            testGroup "collect_type" [
                testCase "test_collect_type_args_0" test_collect_type_args_0,
                testCase "test_collect_type_args_1" test_collect_type_args_1,
                testCase "test_collect_type_args_2" test_collect_type_args_2,
                testProperty "prop_collect_unfolds_0" prop_collect_unfolds_0
            ],
            testGroup "sub_type_type" [
                testCase "test_sub_type_type" test_sub_type_type_0,
                testCase "test_sub_type_type_1" test_sub_type_type_1,
                testProperty "prop_sub_type_type" prop_abstract_to_subs,
                testProperty "prop_sub_type_type_con" prop_abstract_to_subs_con
            ]

    ]
      
test_collect_type_args_0 = actual @?= expected where
    actual   = collect_type_args $ foldr1 AppT expected
    expected = [AppT (ConT $ mkName "Test") (VarT $ mkName "x"), ConT $ mkName "MoreTest"]
   
test_collect_type_args_1 = actual @?= expected where
    actual   = collect_type_args $ foldr1 AppT expected
    expected = [UnboxedTupleT 0, TupleT 3, VarT $ mkName "iz"]
    
test_collect_type_args_2 = 
    assertBool "test_collect_type_args_2" (actual `app_type_iso` expected) where
        actual   = collect_type_args $ foldr1 AppT expected
        expected = [AppT (VarT $ mkName "rx") (VarT $ mkName "ykqtujsxvcrciouq")]

    
prop_collect_unfolds_0 :: [Type] -> Property
prop_collect_unfolds_0 xs = length xs > 0 ==> 
    app_type_iso xs $ collect_type_args $ foldr1 AppT xs

app_type_iso :: [Type] -> [Type] -> Bool
app_type_iso xs ys | length xs == length ys = xs == ys
app_type_iso xs ys | length xs > 1 && xs > ys = all_but_last_two ys xs
app_type_iso xs ys | length ys > 1 && ys > xs = all_but_last_two xs ys
app_type_iso xs ys | otherwise = False

all_but_last_two xs ys = result where
    last_one             = last xs
    first_part_x         = reverse $ drop 1 $ reverse xs
    last_unfolded        = collect_type_args last_one
    
    last_other           = reverse $ take (length last_unfolded) $ reverse ys
    first_part_y         = reverse $ drop (length last_unfolded) $ reverse ys
    
    
    result = (last_unfolded == last_other && first_part_x == first_part_y)
    
------------------------------------------------------------------------------

test_sub_type_type_0 = actual @?= expected where
    actual   = sub_type_type (VarT $ mkName "a") (VarT $ mkName "test") 
                    (AppT (VarT $ mkName "a") (AppT (VarT $ mkName "b") (VarT $ mkName "c")))
    expected = AppT (VarT $ mkName "test") (AppT (VarT $ mkName "b") (VarT $ mkName "c"))

test_sub_type_type_1 = actual @?= expected where
    actual   = sub_type_type (UnboxedTupleT (-1)) (VarT $ mkName "") (UnboxedTupleT (-1)) 
    expected = VarT $ mkName ""

{-    
test_sub_type_type_1 = actual @?= expected where
    actual   = sub_type_type (UnboxedTupleT (-1)) (VarT $ mkName "") (ForallT [] [] (VarT test)) 
    expected = VarT $ mkName ""
-}
    
fromRight (Right x) = x

error_left (Right x) = x
error_left (Left x) = error x

prop_abstract_to_subs :: (Type, Type) -> Gen Bool
prop_abstract_to_subs (typ, old_typ) = do
    typ' <- elements $ universe old_typ
    let abstracted = sub_type_type typ' typ old_typ
        subbed     = sub_type_type typ typ' abstracted    
    return $ old_typ == subbed

prop_abstract_to_subs_con :: (Type, Con) -> Gen Bool
prop_abstract_to_subs_con (typ, con) = do
    typ' <- elements $ concatMap universe $ get_con_types con
    let abstracted = sub_type_con typ' typ con
        subbed     = sub_type_con typ typ' abstracted    
    return $ con == subbed 






