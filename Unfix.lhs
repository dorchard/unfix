> {-# LANGUAGE TemplateHaskell #-} 

> module Unfix where

> import Debug.Trace

> import Language.Haskell.TH.Lib
> import Language.Haskell.TH.Syntax

> import Data.Generics
> import Data.Generics.Uniplate.Data
> import Data.Generics.Uniplate.Operations


> fix f = f (fix f)

> unfix :: Q [Dec] -> Q [Dec]
> unfix qdecs = qdecs >>= gatherM unfix'

> memo :: Q [Dec] -> Q [Dec]
> memo qdecs = do qdecs' <- qdecs
>                 gatherConcatM memofix' qdecs'

> gatherM :: Monad m => ((a, [a]) -> m b) -> [a] -> m [b]
> gatherM _ [] = return []
> gatherM k (x:xs) = (k (x, xs)) >>= (\b -> gatherM k xs >>= (\bs -> return $ b : bs))

> gatherConcatM :: Monad m => ((a, [a]) -> m [b]) -> [a] -> m [b]
> gatherConcatM _ [] = return []
> gatherConcatM k (x:xs) = (k (x, xs)) >>= (\b -> gatherConcatM k xs >>= (\bs -> return $ b ++ bs))

Memofix declarations

> memofix' (f@(SigD n t), rest) = return [f]
> memofix' (f@(FunD n clauses), _) = if (isRecursive f) then 
>                                      do n' <- newName "recf"
>                                         nf <- newName ("u" ++ (nameBase n))
>                                         clauses' <- return $ map (changePat n') clauses
>                                         let memfixcall = AppE (VarE (mkName "memoFix")) (VarE nf)
>                                         let newDec  = FunD n [Clause [] (NormalB memfixcall) []]
>                                         return [newDec, FunD nf (rename n n' clauses')]
>                                    else
>                                      return [FunD n clauses]
> memofix' _ = error "Can't currently 'unfix' anything other than singly recursive functions"

Unfix declarations

> unfix' (f@(SigD n t), rest) = case (lookupRest n rest) of
>                                  Just fun -> if (isRecursive fun) then 
>                                                return $ SigD n (AppT (AppT (ArrowT) t) t)
>                                              else
>                                                return $ SigD n t

> unfix' (f@(FunD n clauses), _) = if (isRecursive f) then 
>                                    do n' <- newName "recf"
>                                       clauses' <- return $ map (changePat n') clauses
>                                       return $ FunD n (rename n n' clauses')
>                                  else
>                                       return $ FunD n clauses
> unfix' _ = error "Can't currently 'unfix' anything other than singly recursive functions"
>     

> changePat n (Clause p b d) = Clause ((VarP n):p) b d

> lookupRest :: Name -> [Dec] -> Maybe Dec
> lookupRest _ [] = Nothing
> lookupRest n (f@(FunD n' _):ds) = if (n == n') then Just f else lookupRest n ds
>                                     

> rename :: (Data a, Typeable a) => Name -> Name -> a -> a
> rename old new = transformBi nameChange
>                    where nameChange (VarE n) = VarE (if (n == old) then new else n)
>                          nameChange x = x

> isRecursive :: Dec -> Bool
> isRecursive (FunD n clauses) = ([v | (VarE v) <- (universeBi clauses), v == n]::[Name]) /= []