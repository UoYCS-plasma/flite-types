module Flite.Pretty where

import Flite.Syntax
import Data.List
import Data.Maybe (fromJust)

consperse :: [a] -> [[a]] -> [a]
consperse x xs = concat (intersperse x xs)

pretty :: Prog -> String
pretty p = "{\n" ++ concatMap show p ++ "}"

{--instance Show Decl where
  show (Func name args rhs) = name ++ " "
                           ++ consperse " " (map showArg args)
                           ++ " = "
                           ++ show rhs ++ ";\n"
 
  --show (Data name args cons)= --name ++ " " 
                          -- ++ consperse " " (map show args)
                          -- ++ " = "
                          -- ++ consperse " " (map show cons) ++ ";\n"

instance Show Exp where
  show (App e es) = consperse " " (showArg e : map showArg es)
  show (PrimApp p es) = "{" ++ show (App (Prim p) es) ++ "}"
  show (Case e as) = "case " ++ show e ++ " of " ++ showBlock showAlt as
  show (Let bs e) = "let " ++ showBlock showBind bs ++ " in " ++ show e
  show (Var v) = v
  show (Fun f) = f
  show (Prim f) = f
  show (Con c) = c
  show (Int i) = show i
  show (Alts as i) = "[" ++ consperse "," as ++ "]"
  show Bottom = "_|_"
  show (Ctr c arity i) = c
  show (Lam vs e) = '\\' : consperse " " vs ++ " -> " ++ show e

showArg :: Exp -> String
showArg (App e []) = showArg e
showArg (App e es) = "(" ++ show (App e es) ++ ")"
showArg (Lam vs e) = "(" ++ show (Lam vs e) ++ ")"
showArg e = show e

showBlock :: (a -> String) -> [a] -> String
showBlock f as = "{ " ++ consperse "; " (map f as) ++ " }"

showAlt :: Alt -> String
showAlt (p, e) = show p ++ " -> " ++ show e

showBind :: Binding -> String
showBind (v, e) = v ++ " = " ++ show e

--}
---Print types
instance Show Type_exp where
  show t =  showWith id (varMap t) t

showWith :: (String -> String) -> [([Int],String)] -> Type_exp -> String
showWith b m (TVAR tvn)      = (fromJust $ lookup tvn m) 
                                --"V" ++ (concat $ map show  tvn )--
showWith b m (TCONS tcn ts)  = 
     case tcn of
     "TArrow" ->  b $ showWith brack m t1 ++ " -> " ++ showWith id m t2
                  where [t1,t2] = ts
     "List"  ->  "[" ++ showWith id m t1 ++ "]"
                  where [t1] = ts
     -- "Pair"  ->  "(" ++ showWith id m t1 ++ "," ++ showWith id m t2 ++ ")"
     --             where [t1,t2] = ts 
     "::"     ->  showWith id m t1 ++ " :: " ++ showWith id m t2  ++ "\n"
                  where [t1,t2] = ts            
     _        ->  if ts==[] then tcn 
                  else tcn ++ " " ++ (concat $ intersperse " " (map (showWith brack m) ts))
     

        
brack :: String -> String
brack s = "("++s++")"

varMap :: Type_exp -> [([Int],String)]
varMap t = zip (nub $ varsOf t) varNames
  where
  varNames = map (:[]) lett ++ concatMap (\n -> [c : show n | c <- lett]) [1..]
  lett = ['a'..'z']

varsOf :: Type_exp -> [[Int]]
varsOf (TVAR tvn) = [tvn]
varsOf (TCONS tcn ts) = concatMap varsOf ts 

--}
showfuntypes (fts,decls) =  (map showfuntype fts , decls )
         
showfuntype (f,t) =  (f,t)--f ++ " :: " ++ (show t) ++ "\n"
 
