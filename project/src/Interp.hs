-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M
import Data.List (find)

import Types

eval :: Expr -> Env -> Value
eval (ENum i)    _   = VInt i
eval (EVar x)    env = case M.lookup x env of
  Just v  -> v
  Nothing -> error ("Unbound variable: " ++ x)
eval (EPack t a) _   = VPack t a []
eval (ELam ps b) env = VClos ps b env
eval (EAp f x)   env = apply (eval f env) (eval x env)
eval (EBinop n)  _   = VPrim n []
eval (ELet False defs body) env =
  let env' = foldl (\e (n, rhs) -> M.insert n (eval rhs e) e) env defs
  in eval body env'
eval (ELet True defs body) env =
  let env'  = M.union defs' env
      defs' = M.fromList [(n, eval rhs env') | (n, rhs) <- defs]
  in eval body env'
eval (ECase scrut alts) env =
  case eval scrut env of
    VPack tag _ args ->
      case find (\(t, _, _) -> t == tag) alts of
        Just (_, params, body) ->
          let bind e (n, v) = if n == "_" then e else M.insert n v e
              env'          = foldl bind env (zip params args)
          in eval body env'
        Nothing -> error ("case: no matching alt for tag " ++ show tag)
    other -> error ("case: scrutinee is not a constructor: " ++ show other)

apply :: Value -> Value -> Value
apply (VClos (p:ps) body env) v =
  let env' = M.insert p v env
  in if null ps then eval body env' else VClos ps body env'
apply (VClos [] _ _) _ =
  error "apply: closure already saturated"
apply (VPack t a args) v = VPack t a (args ++ [v])
apply (VPrim n args) v =
  let args' = args ++ [v]
  in if length args' == 2 then applyPrim n args' else VPrim n args'
apply other _ =
  error ("apply: cannot apply non-function value: " ++ show other)

applyPrim :: Name -> [Value] -> Value
applyPrim "+"  [VInt a,  VInt b]  = VInt  (a + b)
applyPrim "-"  [VInt a,  VInt b]  = VInt  (a - b)
applyPrim "*"  [VInt a,  VInt b]  = VInt  (a * b)
applyPrim "/"  [VInt a,  VInt b]  = VInt  (a `div` b)
applyPrim "<"  [VInt a,  VInt b]  = VBool (a <  b)
applyPrim "<=" [VInt a,  VInt b]  = VBool (a <= b)
applyPrim ">"  [VInt a,  VInt b]  = VBool (a >  b)
applyPrim ">=" [VInt a,  VInt b]  = VBool (a >= b)
applyPrim "==" [VInt a,  VInt b]  = VBool (a == b)
applyPrim "~=" [VInt a,  VInt b]  = VBool (a /= b)
applyPrim "&"  [VBool a, VBool b] = VBool (a && b)
applyPrim "|"  [VBool a, VBool b] = VBool (a || b)
applyPrim n    args               =
  error ("applyPrim: bad args for " ++ n ++ ": " ++ show args)

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_, [], mainBody) ->
      case eval mainBody globalEnv of
        VInt  i -> show i
        VBool b -> show b
        v       -> show v
    Just _ -> error "Supercombinator main must take no arguments."
  where
    globalEnv = M.fromList
      [ (name, mkSc params body)
      | (name, (_, params, body)) <- M.toList prog
      ]
    mkSc []     body = eval body globalEnv
    mkSc params body = VClos params body globalEnv
