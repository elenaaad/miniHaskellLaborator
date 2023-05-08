module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var s) = IndexedVar {ivName=s, ivCount=0}

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar name count)
    | count == 0 = (Var {getVar = name})
    | otherwise = (Var {getVar = name ++ "_" ++ (show count)})

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.

desugarExp :: ComplexExp -> Exp
desugarExp (CX x) = X (desugarVar x)
desugarExp (CLam x e) = Lam (desugarVar x) (desugarExp e)
desugarExp (CApp e1 e2) = App (desugarExp e1) (desugarExp e2)
desugarExp (Let x ex e) = App (Lam (desugarVar x) (desugarExp e)) (desugarExp ex)
desugarExp (LetRec f ef e)
  = desugarExp (Let f (CApp (CX (Var "fix")) (CLam f ef)) e)
desugarExp (List ces)
--  = foldr ((App . App consExp) . desugarExp) nilExp ces
  = foldr cons nilExp (map desugarExp ces)
  where
    cons e l = App (App consExp e) l
desugarExp (Nat n)
  = foldr successor zeroExp (replicate (fromIntegral n) ())
  where
    successor _ n = App succExp n

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp (X v) = (CX (sugarVar v))
sugarExp (App cex1 cex2) = (CApp (sugarExp cex1) (sugarExp cex2))
sugarExp (Lam v cex) = (CLam (sugarVar v) (sugarExp cex))

-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))
