module Language.While.Semantics
       ( emptyEnv
       , updateBinding
       , lookupBinding
       , denotA
       , denotB
       , operBS
       , operSS
       , operSSUntilStuck
       ) where

import           Control.Applicative   ((<$>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import qualified Language.While.Syntax as S

type Env = M.Map S.Name Int

emptyEnv :: Env
emptyEnv = M.empty

updateBinding :: S.Name -> Int -> Env -> Env
updateBinding = M.insert

lookupBinding :: S.Name -> Env -> Int
lookupBinding = M.findWithDefault 42

denotA :: S.AExp -> Env -> Int
denotA a env = case a of
  S.Lit n -> n
  S.Var name -> lookupBinding name env
  S.Minus x y -> denotA x env - denotA y env
  S.Multiply x y -> denotA x env * denotA y env

denotB :: S.BExp -> Env -> Bool
denotB b env = case b of
  S.Tr -> True
  S.LEQ x y -> denotA x env <= denotA y env
  S.Not b' -> not (denotB b' env)
  S.And x y -> denotB x env && denotB y env

operBS :: S.Com -> Env -> Env
operBS c env = case c of
  S.Skip -> env
  S.Assignment name a -> updateBinding name (denotA a env) env
  S.Sequence c1 c2 -> operBS c2 . operBS c1 $ env
  S.If b t e
    | denotB b env -> operBS t env
    | otherwise    -> operBS e env
  w@(S.While b body)
    | denotB b env -> operBS w . operBS body $ env
    | otherwise    -> env


operSS :: (S.Com, Env) -> Maybe (S.Com, Env)
operSS (c, env) = case c of
  S.Skip -> Nothing
  S.Assignment name a -> Just (S.Skip, updateBinding name (denotA a env) env)
  S.Sequence S.Skip c2 -> Just (c2, env)
  S.Sequence c1 c2 -> do
    (c1', env') <- operSS (c1, env)
    return (S.Sequence c1' c2, env')
  S.If b t e
    | denotB b env -> Just (t, env)
    | otherwise    -> Just (e, env)
  w@(S.While b body) -> Just (S.If b (S.Sequence body w) S.Skip, env)


operSSUntilStuck :: (S.Com, Env) -> (S.Com, Env)
operSSUntilStuck step =
  fromMaybe step (operSSUntilStuck <$> operSS step)
