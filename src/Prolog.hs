module Prolog where

import Prolog.Parser

data Id = Id String deriving (Eq, Show)

data Term = TNum Integer
          | TVar Id
          | TPred Pred
          deriving (Eq, Show)

data Pred = Pred
    { getId :: Id
    , getArgs :: [Term]
    } deriving (Eq, Show)

data Clause = Clause Pred [Pred] deriving (Eq, Show)

type Sub = Id -> Maybe Term
type Answer = Bool
type FailureK = () -> Answer
type SuccessK = Sub -> FailureK -> Answer
type UnifySuccessK = Sub -> Answer

initSub :: Sub
initSub _ = Nothing

initSuccessK :: SuccessK
initSuccessK _ _ = True

initFailureK :: FailureK
initFailureK _ = False

proveQuery :: [Pred] -> [Clause] -> Answer
proveQuery pi delta = proveAll pi initSub delta initSuccessK initFailureK

proveAll :: [Pred] -> Sub -> [Clause] -> SuccessK -> FailureK -> Answer
proveAll [] theta delta ks kf = ks theta kf
proveAll (p : pi) theta delta ks kf =
    prove p theta delta (\theta' kf' -> proveAll pi theta' delta ks kf') kf

prove :: Pred -> Sub -> [Clause] -> SuccessK -> FailureK -> Answer
prove p theta delta ks kf = proveGoal delta p theta delta ks kf

proveGoal :: [Clause] -> Pred -> Sub -> [Clause] -> SuccessK -> FailureK -> Answer
proveGoal [] p theta delta ks kf = kf ()
proveGoal (d : delta') p theta delta ks kf =
    let (Clause p' pi) = copy d
        kf' = \() -> proveGoal delta' p theta delta ks kf
     in unify p' p theta (\theta' -> proveAll pi theta' delta ks kf') kf'

copy :: Clause -> Clause
copy = undefined

unify :: Pred -> Pred -> Sub -> UnifySuccessK -> FailureK -> Answer
unify = undefined

prolog = undefined
