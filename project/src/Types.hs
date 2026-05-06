-- | Here are the types for the Core interpreter.
-- You may add constructors if you wish, but do not modify any that are already here.

module Types where

import qualified Data.HashMap.Lazy as M

type Name = String
type IsRec = Bool

data Expr = EVar Name                  -- Variables
          | ENum Int                   -- Numbers
          | EPack Int Int              -- Constructors
          | EAp Expr Expr              -- Applications
          | ELet
               IsRec                   -- is the let recursive?
               [(Name, Expr)]          -- Local Variable definitions
               Expr                    -- Body of the let
          | ECase                      -- Case Expressions
               Expr
               [(Int, [Name], Expr)]   -- Alternatives
          | ELam [Name] Expr           -- Functions (Lambdas)
          | EBinop Name                -- Built-in binary operator (e.g. "+", "&", "==")
   deriving (Eq, Show)

type Decl = (Name, [Name], Expr)       -- The name, parameter list, and body of a supercombinator declaration

type Core = M.HashMap Name Decl        -- A core program is an environment of declarations

-- Runtime values produced by the interpreter.
data Value = VInt  Int
           | VBool Bool
           | VPack Int Int [Value]     -- tag, arity, args collected so far
           | VClos [Name] Expr Env     -- params still to bind, body, captured env
           | VPrim Name [Value]        -- built-in op name, args collected so far
   deriving (Show)

type Env = M.HashMap Name Value
