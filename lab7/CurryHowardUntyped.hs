import Prelude (undefined)
import Distribution.Compat.Lens (_1)

newtype False = False { getFalse :: forall t. t }
newtype True = True { getTrue :: forall t . t -> t }
newtype And a b = And { getAnd :: forall t. (a -> b -> t) -> t }
newtype Or a b = Or { getOr :: forall t . (a -> t) -> (b -> t) -> t}
type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)


-- Natural deduction introduction and elimination rules

trueIntro :: True                                   -- true introduction
trueIntro = True (\x -> x)

falseElim :: False -> b                             -- false elimination
falseElim f = getFalse f 

implIntro :: (a -> b) -> (a -> b)                   -- implication introduction
implIntro f x = f x

implElim :: (a -> b) -> a -> b                      -- implication elimination
implElim f x = f x

andIntro :: a -> b -> And a b                       -- and introduction
andIntro x y = And (\f -> f x y)

andElimL :: And a b -> a                            -- and elimination 1
andElimL ab = getAnd ab (\a _ -> a)

andElimR :: And a b -> b                            -- and elimination 2
andElimR ab = getAnd ab (\_ b -> b)

orIntroL :: a -> Or a b                             -- or introduction 1
orIntroL x = Or (\f _ -> f x)

orIntroR :: b -> Or a b                             -- or introduction 2
orIntroR y = Or (\_ g -> g y)

orElim :: Or a b -> (a -> c) -> (b -> c) -> c       -- or elimination
orElim (Or f _) g _ = f g

notElim :: Not p -> p -> c                          -- not elimination 
notElim f x = case f x of False -> undefined

notIntro :: (forall p. a -> p) -> Not a             -- not introduction
notIntro f x = False (f x)

iffIntro :: (a -> b) -> (b -> a) -> Iff a b         -- iff introduction
iffIntro f g = And f g

iffElimL :: Iff a b -> a -> b                       -- iff elimination 1
iffElimL (And f _) x = f x

iffElimR :: Iff a b -> b -> a                       -- iff elimination 1
iffElimR (And _ g) y = g y

-- Hilbert-style axiomatization for intuitionistic propositional logic

ax1 :: a -> b -> a
ax1 x _ = x

ax2 :: (a -> b) -> (a -> (b -> c)) -> a -> c
ax2 f g x = g x (f x)

ax3 :: a -> b -> And a b
ax3 x y = And (\f -> f x y)

ax4 :: And a b -> a
ax4 ab = getAnd ab (\a _ -> a)

ax5 :: And a b -> b
ax5 ab = getAnd ab (\_ b -> b)

ax6 :: a -> Or a b
ax6 x = Or (\f _ -> f x)

ax7 :: b -> Or a b
ax7 y = Or (\_ g -> g y)

ax8 :: (a -> c) -> (b -> c) -> Or a b -> c
ax8 f _ (Or g h) = g f h

ax9 :: (a -> b) -> (a -> Not b) -> Not a
ax9 f g x = case g x of False -> undefined

ax10 :: Not a -> a -> b
ax10 f x = case f x of False -> undefined

modusPonens :: (a -> b) -> a -> b
modusPonens f x = f x

-- Several tautologies

pNPFalse :: p -> Not p -> False
pNPFalse _ f = case f of False -> undefined

deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 (And np nq) (Or f g) = np (\p -> f p) (nq (\q -> g q))

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 f = And (\g -> f (Or g (\_ -> False)))

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 (Or np nq) (And p q) = case np p of False -> case nq q of False -> undefined

excludedMiddleImplDoubleNeg :: Or a (Not a) -> (Not (Not a) -> a)
excludedMiddleImplDoubleNeg (Or f g) dn = case dn (\x -> f x) of False -> g (getFalse . dn)

doubleNegImplExcludedMiddle :: (forall a. Not (Not a) -> a) -> Or b (Not b)
doubleNegImplExcludedMiddle dn = Or (\_ -> getFalse (dn (\_ -> undefined))) (\g -> g)   