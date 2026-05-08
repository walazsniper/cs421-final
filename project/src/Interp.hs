-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M

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
eval _           _   = error "Interp: eval not yet implemented for this Expr"

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
      case eval mainBody M.empty of
        VInt  i -> show i
        VBool b -> show b
        v       -> show v
    Just _ -> error "Supercombinator main must take no arguments."
