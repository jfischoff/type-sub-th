module Language.Haskell.TH.TypeSub where
import Language.Haskell.TH
import Control.Arrow
import Data.List
import Data.Generics.Uniplate.Data
import Control.Applicative
import Control.Monad
import Debug.Trace.Helpers
import Data.Tuple.Select

type Result a = Either String a

--replace the binpath with just a type

-- | Create a new data declaration where the type variables have been subsituted with the 
-- | supplied types.
--Todo
--this function can return an error. Should be an either
--all of the errors can be consolidating
--additionally I should make the other direction
subst_types :: Dec -> [Type] -> Result Dec
subst_types dec types  = do
    let sub_type' dec (n, t) = sub_type dec (VarT n) t
    names <- mapM (get_ty_var_name dec) [0..length types - 1] 
    foldM sub_type' dec $ zip names types 

ty_var_name :: TyVarBndr -> Name
ty_var_name (KindedTV name _ ) = name
ty_var_name (PlainTV name) = name

modify_cons :: Dec -> ([Con] -> [Con]) -> Dec
modify_cons (NewtypeD x y z con w) f = result where
     h = f [con]
     result = (NewtypeD x y z (head h) w)
modify_cons (DataD x y z cons w)   f = result where
    cs = f cons
    result = (DataD x y z cs w)
    
get_cons :: Dec -> [Con]
get_cons (NewtypeD x y z con w) = [con]
get_cons (DataD x y z cons w)   = cons

set_cons :: Dec -> [Con] -> Dec
set_cons (NewtypeD x y z _ w) [con] = (NewtypeD x y z con w) 
set_cons (DataD x y z _ w) cons = (DataD x y z cons w)  

get_ty_vars :: Dec -> [TyVarBndr]
get_ty_vars (NewtypeD _ _ ty_vars _ _) = ty_vars
get_ty_vars (DataD    _ _ ty_vars _ _) = ty_vars

get_ty_var_name :: Dec -> Int -> Result Name
get_ty_var_name dec i = ty_var_name <$> get_value (get_ty_vars dec) i

get_value :: [a] -> Int -> Result a
get_value xs i | i < length xs = Right $ xs !! i
get_value xs i | otherwise     = Left "Index out of bounds"

sub_type :: Dec -> Type -> Type -> Result  Dec 
sub_type dec typ typ' = Right $ modify_cons dec (map (sub_type_con typ typ')) 

thirdM :: Monad m => (t -> m t3) -> (t1, t2, t) -> m (t1, t2, t3) 
thirdM f (x,y,z) = do
    z' <- f z
    return (x,y, z')

secondM :: Monad m => (t -> m t2) -> (t1, t) -> m (t1, t2) 
secondM f (x,y) = do
    y' <- f y
    return (x,y')

firstM :: Monad m => (t -> m t1) -> (t, t2) -> m (t1, t2) 
firstM f (x,y) = do
    x' <- f x
    return (x', y)
    
third f (x, y, z) = (x, y, f z)
    
zipM t = fmap (\(x, y) -> zip x y) t
zipM3 t = fmap (\(x, y, z) -> zip3 x y z) t

get_con_types :: Con -> [Type]
get_con_types (NormalC n st) = map snd st
get_con_types (RecC n st) = map sel3 st
get_con_types (InfixC x n y) = map snd [x, y]
get_con_types (ForallC t cxt con) = get_con_types con

modify_types :: Con -> ([Type] -> [Type]) -> Con
modify_types (NormalC n strict_types)  f = NormalC n $ uncurry zip $ (second f $ unzip strict_types)
modify_types (RecC n var_strict_types) f = RecC    n $ (\(x, y, z) -> zip3 x y z) (third f $ unzip3 var_strict_types )
modify_types (InfixC x n y) f = result where
    [x', y'] = uncurry zip $ second f $ unzip [x, y]

    result = InfixC x' n y'
modify_types (ForallC t cxt con) f = ForallC t cxt $ modify_types con f
    
sub_type_con :: Type -> Type -> Con -> Con
sub_type_con typ typ' con = modify_types con (map (sub_type_type typ typ'))
    
has_var :: Name -> Type -> Bool
has_var name typ = any (name==)  [ n | VarT n <- universe typ]

sub_type_type :: Type -> Type -> Type -> Type
sub_type_type ty typ old_typ = transform sub_type_type' old_typ where
    sub_type_type' t | t == ty = typ
    sub_type_type' x | otherwise = x

has_type :: Type -> Type -> Bool
has_type typ_to_find typ = any (typ_to_find==) $ universe typ
                  
collect_type_args :: Type -> [Type]
collect_type_args (AppT x y) = x:(collect_type_args y)
collect_type_args x          = [x]

reduce_type :: Name -> Type -> Q (Result Dec)
reduce_type name app_tys = do
    TyConI dec <- reify name
    let args = collect_type_args app_tys
    return $ subst_types dec args








