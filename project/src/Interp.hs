-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M

import Types

eval :: Expr -> Env -> Value
eval (ENum i) _ = VInt i
eval _        _ = error "Interp: eval not yet implemented for this Expr"

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
