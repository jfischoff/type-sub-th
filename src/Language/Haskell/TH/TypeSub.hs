-- | Subsitute one type for another in Template Haskell Dec's, Con's and Type's.
-- | Warning! 
-- | There are a few known issues. The types in cxt's are not subsistuted.
-- | Additionally, the Dec's type variables are regenerated after subistution 
-- | and all explicitly kinded type variables are converted to implicitly kinded type
-- | variables.
module Language.Haskell.TH.TypeSub (
 sub_types_dec, 
 sub_type_dec, 
 sub_type_con, 
 sub_type,
 has_type,
 get_cons,
 get_con_types,
 update_ty_vars,
 Result) where
import Language.Haskell.TH
import Control.Arrow
import Data.List
import Data.Generics.Uniplate.Data
import Control.Applicative
import Control.Monad
import Data.Tuple.Select

-- | A result for partial functions
type Result a = Either String a

-- | Create a new data declaration where the type variables have been subsituted with the 
-- | supplied types. Returns an error if the more types the types are provided.
sub_types_dec :: [Type] -> Dec -> Result Dec
sub_types_dec types dec = do
    let sub_type_dec' dec' (n, t) = sub_type_dec t (VarT n) dec'
    names <- mapM (get_ty_var_name dec) [0..length types - 1] 
    return $ foldl' sub_type_dec' dec $ zip names types 
    
-- | Substitute the a new type for an existing type in all the constructors in a Dec.
-- | If the type to replace is missing, the function does nothing. 
sub_type_dec :: Type -> Type -> Dec -> Dec 
sub_type_dec new_type old_type dec = update_ty_vars $ 
    modify_cons dec (map (sub_type_con new_type old_type)) 
    
-- | Substitute the new type for the old type in the constructor       
sub_type_con :: Type -> Type -> Con -> Con
sub_type_con new_type old_type con = modify_types con (map (sub_type new_type old_type))

-- | Substitute the new type for the old type in the type  
-- transform is from Uniplate     
sub_type :: Type -> Type -> Type -> Type
sub_type new_type old_type input = transform sub_type_type' input where
     sub_type_type' t | t == old_type = new_type
     sub_type_type' x | otherwise = x

------------------------------------------------------------------------------------------------
--Various helper functions that will get moved somewhere else eventually

ty_var_name :: TyVarBndr -> Name
ty_var_name (KindedTV name _ ) = name
ty_var_name (PlainTV name) = name

modify_cons :: Dec -> ([Con] -> [Con]) -> Dec
modify_cons (NewtypeD x y z con w) f     = NewtypeD x y z (head $ f [con]) w
modify_cons (DataD x y z cons w)   f     = DataD x y z (f cons) w
modify_cons (DataInstD x y z cons w) f   = DataInstD x y z (f cons) w
modify_cons (NewtypeInstD x y z con w) f = NewtypeInstD x y z (head $ f [con]) w
modify_cons x _                          = x

get_cons :: Dec -> [Con]
get_cons (NewtypeD _ _ _ con _)      = [con]
get_cons (DataD _ _ _ cons _)        = cons
get_cons (DataInstD _ _ _ cons _)    = cons
get_cons (NewtypeInstD _ _ _ con _)  = [con]
get_cons _                           = []
 
get_ty_vars :: Dec -> [TyVarBndr]
get_ty_vars (NewtypeD _ _ ty_vars _ _) = ty_vars
get_ty_vars (DataD    _ _ ty_vars _ _) = ty_vars
get_ty_vars (TySynD _ ty_vars _)       = ty_vars
get_ty_vars (ClassD _ _ ty_vars _ _)   = ty_vars
get_ty_vars (FamilyD _ _ ty_vars _ )   = ty_vars
get_ty_vars _                          = []

set_ty_vars :: Dec -> [TyVarBndr] -> Dec
set_ty_vars (NewtypeD x y _ z w)  ty_vars = NewtypeD x y ty_vars z w
set_ty_vars (DataD    x y _ z w)  ty_vars = DataD x y ty_vars z w
set_ty_vars (TySynD x _ y)        ty_vars = TySynD x ty_vars y
set_ty_vars (ClassD x y _ z w)    ty_vars = ClassD x y ty_vars z w
set_ty_vars (FamilyD x y _ z )    ty_vars = FamilyD x y ty_vars z 
set_ty_vars x _                           = x


get_type_name :: Type -> Result Name
get_type_name (ForallT _ _ typ) = get_type_name typ
get_type_name (VarT n)          = Right n
get_type_name (ConT n)          = Right n
get_type_name x                 = Left ("No name for " ++ show x)

from_right :: Result a -> a
from_right (Right x) = x
from_right (Left x)  = error $ x ++ " is not Right!"

get_ty_var_name :: Dec -> Int -> Result Name
get_ty_var_name dec i = ty_var_name <$> get_value (get_ty_vars dec) i

get_value :: [a] -> Int -> Result a
get_value xs i | i < length xs = Right $ xs !! i
get_value _ i | otherwise     = Left $ show i ++ " Index out of bounds"

collect_vars :: Type -> [Type]
collect_vars typ = [VarT n | VarT n <- universe typ]

make_ty_vars :: Type -> [TyVarBndr]
make_ty_vars = map (PlainTV . from_right . get_type_name) . nub . collect_vars

update_ty_vars :: Dec -> Dec 
update_ty_vars (TySynD n _ t) = (TySynD n (make_ty_vars t) t)
update_ty_vars dec = set_ty_vars dec $ concatMap make_ty_vars $ 
    concatMap (get_con_types) $ get_cons dec

    
third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f (x, y, z) = (x, y, f z)
    
get_con_types :: Con -> [Type]
get_con_types (NormalC _ st) = map snd st
get_con_types (RecC _ st) = map sel3 st
get_con_types (InfixC x _ y) = map snd [x, y]
get_con_types (ForallC _ _ con) = get_con_types con

modify_types :: Con -> ([Type] -> [Type]) -> Con
modify_types (NormalC n strict_types)  f = NormalC n $ uncurry zip $ (second f $ unzip strict_types)
modify_types (RecC n var_strict_types) f = RecC    n $ (\(x, y, z) -> zip3 x y z) (third f $ unzip3 var_strict_types )
modify_types (InfixC x n y) f = result where
    [x', y'] = uncurry zip $ second f $ unzip [x, y]
    result = InfixC x' n y'
modify_types (ForallC t context con) f = ForallC t context $ modify_types con f
       
has_var :: Name -> Type -> Bool
has_var name typ = any (name==)  [ n | VarT n <- universe typ]

has_type :: Type -> Type -> Bool
has_type typ_to_find typ = any (typ_to_find==) $ universe typ
                  











