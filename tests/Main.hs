{-# LANGUAGE TupleSections #-}
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
import Control.Applicative ((<$>))
    
main = defaultMainWithArgs tests ["-a 100", "-o 5"]

tests = [
            testGroup "sub_type" [
                testCase "test_sub_type" test_sub_type_0,
                testCase "test_sub_type_1" test_sub_type_1,
                testCase "test_sub_type_2" test_sub_type_2,
                testCase "test_sub_type_3" test_sub_type_3,
                testCase "test_sub_type_4" test_sub_type_4,
                testProperty "prop_sub_type" prop_abstract_to_subs
            ],
            testGroup "sub_type_con" [
                testCase "test_sub_type_con_0" test_sub_type_con_0,
                testProperty "prop_sub_type_con" prop_abstract_to_subs_con
            ],
            testGroup "sub_type_dec" [
                testCase "test_sub_type_dec_0" test_sub_type_dec_0,
                testProperty "prop_sub_type_dec" prop_abstract_to_subs_dec
            ],
            testGroup "sub_types_dec" [
                testCase "test_sub_types_dec_0" test_sub_types_dec_0
            ]

    ]
      
------------------------------------------------------------------------------

test_sub_type_0 = actual @?= expected where
    actual   = sub_type (VarT $ mkName "test") (VarT $ mkName "a")
                    (AppT (VarT $ mkName "a") (AppT (VarT $ mkName "b") (VarT $ mkName "c")))
    expected = AppT (VarT $ mkName "test") (AppT (VarT $ mkName "b") (VarT $ mkName "c"))

test_sub_type_1 = actual @?= expected where
    actual   = sub_type (VarT $ mkName "") (UnboxedTupleT (-1)) (UnboxedTupleT (-1)) 
    expected = VarT $ mkName ""

    
test_sub_type_2 = actual @?= expected where
    actual   = sub_type ListT ArrowT $ sub_type ArrowT ListT (ForallT [] [] ListT)
    expected = (ForallT [] [] ListT)

test_sub_type_3 = actual @?= expected where
    actual   = sub_type (AppT ArrowT ArrowT) ArrowT $ 
               sub_type ArrowT (AppT ArrowT ArrowT) expected
    expected = AppT (AppT (UnboxedTupleT 27799) (UnboxedTupleT (-41693))) (AppT ArrowT ArrowT)    
    
test_sub_type_4 = actual @?= expected where
    actual   = sub_type (AppT (TupleT 12) (ForallT [] [] ListT)) ListT $
               sub_type ListT (AppT (TupleT 12) (ForallT [] [] ListT)) expected
    expected = AppT (SigT (AppT (ConT $ mkName "Y24li12988h5rgE7kC") (TupleT 7)) 
                    (ArrowK StarK StarK)) 
                    (AppT (TupleT 12) (ForallT [] [] ListT))
                    

test_sub_type_con_0  = actual @?= expected where
    actual   = sub_type_con new_type old_type constr
    expected = NormalC (mkName "hey") $ map (NotStrict,) [new_type, VarT $ mkName "w", new_type]
    constr   = NormalC (mkName "hey") $ map (NotStrict,) [old_type, VarT $ mkName "w", old_type]
    old_type = AppT (ConT $ mkName "Int") (VarT $ mkName "a")
    new_type = ArrowT
    
test_sub_type_dec_0  = actual @?= expected where
    actual        = sub_type_dec new_type old_type dec
    expected      = DataD [] name [PlainTV $ mkName "a"] expected_cons 
    dec           = DataD [] name [] cons
    expected_cons = undefined
    cons          = undefined
    old_type      = AppT (ConT $ mkName "Int") (VarT $ mkName "a")
    new_type      = TupleT 10        
         
test_sub_types_dec_0 = undefined

fromRight (Right x) = x

error_left (Right x) = x
error_left (Left x) = error x

traceItNote x y = trace (x ++ show y) y

prop_abstract_to_subs :: Gen Bool
prop_abstract_to_subs = do 
    old_typ <- arbitrary
    typ  <- suchThat arbitrary (not . (flip has_type) old_typ)
    typ' <- elements $ universe old_typ
    let abstracted = sub_type typ typ' old_typ
        subbed     = sub_type typ' typ abstracted    
    return $ old_typ == subbed

prop_abstract_to_subs_con :: Gen Bool
prop_abstract_to_subs_con = do
    con <- arbitrary
    typ  <- suchThat arbitrary (\x -> all (not . (has_type x)) (get_con_types con))
    typ' <- elements $ concatMap universe $ get_con_types con
    let abstracted = sub_type_con typ typ' con
        subbed     = sub_type_con typ' typ abstracted    
    return $ con == subbed 


prop_abstract_to_subs_dec :: Gen Bool
prop_abstract_to_subs_dec = do
    dec <- update_ty_vars <$> haskell_98_data_dec primitive_types
    typ  <- suchThat arbitrary (\x -> all (not . (has_type x)) (concatMap get_con_types $ get_cons dec))
    typ' <- elements $ concatMap universe $ concatMap get_con_types $ get_cons dec
    let abstracted = sub_type_dec typ typ' dec
        subbed     = sub_type_dec typ' typ abstracted
    return $ dec == subbed



