{-# LANGUAGE TemplateHaskell #-} 

module Language.Haskell.Unfix(fix, unfix, refix) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

-- Standard fixed point combinator
fix f = f (fix f)

unfix :: Q [Dec] -> Q [Dec]
unfix qdecs = qdecs >>= gatherM unfix'

refix :: Q Exp -> Q [Dec] -> Q [Dec]
refix fixP qdecs = do qdecs' <- qdecs
                      fixP'  <- fixP
                      gatherConcatM (refix' fixP') qdecs'

-- Monadic cobind-like operation 

gatherM :: Monad m => ((a, [a]) -> m b) -> [a] -> m [b]
gatherM _ [] = return []
gatherM k (x:xs) = (k (x, xs)) >>= (\b -> gatherM k xs >>= (\bs -> return $ b : bs))

gatherConcatM :: Monad m => ((a, [a]) -> m [b]) -> [a] -> m [b]
gatherConcatM _ [] = return []
gatherConcatM k (x:xs) = (k (x, xs)) >>= (\b -> gatherConcatM k xs >>= (\bs -> return $ b ++ bs))

-- Unfix declarations

unfix' :: (Dec, [Dec]) -> Q (Dec)
unfix' (f@(SigD n t), rest) = case (lookupRest n rest) of
                                 Just fun -> if (isRecursive fun) then 
                                               return $ SigD n (unfixType t)
                                             else
                                               return $ SigD n t

unfix' (f@(FunD n clauses), _) = if (isRecursive f) then 
                                   do n' <- newName ("recf" ++ (nameBase n))
                                      clauses' <- return $ map (changePat n') clauses
                                      return $ FunD n (rename n n' clauses')
                                 else
                                      return $ FunD n clauses
unfix' _ = error "Can't currently 'unfix' anything other than singly recursive functions"


-- Generate the type of the 'unfix'ed function
unfixType :: Type -> Type
unfixType (ForallT binders ctx typ) = ForallT binders ctx (AppT (AppT ArrowT typ) typ)
unfixType t                          = AppT (AppT ArrowT t) t
    
-- Refix with the 'fixP' operator a declaration

refix' :: Exp -> (Dec, [Dec]) -> Q [Dec]
refix' _    (f@(SigD n t), rest) = return [f]
refix' fixP (f@(FunD n clauses), _) = if (isRecursive f) then 
                                     do n' <- newName ("recf" ++ (nameBase n))
                                        nf <- newName ("u" ++ (nameBase n))
                                        clauses' <- return $ map (changePat n') clauses
                                        let memfixcall = AppE fixP (VarE nf)
                                        let newDec  = FunD n [Clause [] (NormalB memfixcall) []]
                                        return [newDec, FunD nf (rename n n' clauses')]
                                   else
                                     return [FunD n clauses]
refix' _ _ = error "Can't currently 'unfix' anything other than singly recursive functions"


changePat n (Clause p b d) = Clause ((VarP n):p) b d

lookupRest :: Name -> [Dec] -> Maybe Dec
lookupRest _ [] = Nothing
lookupRest n (f@(FunD n' _):ds) = if (n == n') then Just f else lookupRest n ds
                                     

rename :: (Data a, Typeable a) => Name -> Name -> a -> a
rename old new = transformBi nameChange
                   where nameChange (VarE n) = VarE (if (n == old) then new else n)
                         nameChange x = x

isRecursive :: Dec -> Bool
isRecursive (FunD n clauses) = ([v | (VarE v) <- (universeBi clauses), v == n]::[Name]) /= []