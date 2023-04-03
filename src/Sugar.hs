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
desugarExp (CX v) = (X (desugarVar v))
desugarExp (CLam v cex) = (Lam (desugarVar v) (desugarExp cex))
desugarExp (CApp cex1 cex2) = (App (desugarExp cex1) (desugarExp cex2))
desugarExp (Nat n)
    | n == 0 = (X (makeIndexedVar "Z"))
    | otherwise = (App (X (makeIndexedVar "S")) (desugarExp (Nat (n-1))))
desugarExp (List []) = (X (makeIndexedVar "Nil"))
desugarExp (List (h:t)) = (App (App (X (makeIndexedVar ":")) (desugarExp h)) (desugarExp (List t)))
desugarExp (Let v cex1 cex2) = (App (Lam (desugarVar v) (desugarExp cex2)) (desugarExp cex1))
desugarExp (LetRec v cex1 cex2) = (App (Lam (desugarVar v) (desugarExp cex2)) (App (X (makeIndexedVar "fix")) (Lam (desugarVar v) (desugarExp cex1))))

-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp (X v) = (CX (sugarVar v))
sugarExp (App cex1 cex2) = (CApp (sugarExp cex1) (sugarExp cex2))
sugarExp (Lam v cex) = (CLam (sugarVar v) (sugarExp cex))

-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 