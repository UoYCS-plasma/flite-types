--Compiler for version 3 of reduceron. FP '10 Paper
module Flite.RedCompile where
import Flite.Syntax
import Flite.Flatten
import Flite.RedFrontend
import Data.List
import Flite.Traversals
import Flite.Inline
import qualified Flite.RedSyntax as R
import Flite.Pretty
import Debug.Trace

redCompile :: Prog -> [(String,R.Template)]
redCompile = translate

translate :: Prog -> [(String,R.Template)]
translate p = map ( trDefn p2) p2
  where
    p0 = frontend p
    p1 = [ (f, [v | Var v <- args], flatten rhs)
         | Func f args rhs <- p0
         ]
    p2 = lift "main" p1
    lift f p = xs ++ ys
     where (xs, ys) = partition (\(g, _, _) -> f == g) p

trDefn p (f, args, (_, spine):bindings) =
     (f,(length args, trApp s spine, map (trApp s) es))
  where
    es = [e | (v, e) <- bindings]
    
    s = [(x, R.ARG i) | (x, i) <- zip args [0..]]
     ++ [(v, R.PTR i) | ((v, e), i) <- zip bindings [0..]]
    
    trApp s es =  concat $ map (trExp s) es

    trExp s (Int n) = [R.INT n]
    trExp s (Prim p) = [R.PRI p]
    trExp s (Var v) =
      case lookup v s of
        Nothing -> error ("RedCompile.hs: undefined variable " ++ v)
        Just atom -> [atom]
    trExp s (Ctr c arity index) = [R.CON arity index]
    trExp s (Fun f)
      | isPrimId f = [R.PRI f]
      | otherwise = [R.FUN (length $ funArgs d) i]
      where (d, i) = lookupFun f
    trExp s (Alts (f:fs) k) = [R.TAB i (length (f:fs)) k]
      where (d, i) = lookupFun f
    trExp s (Bottom) = []  -- Undefined  
   
    funName (f, args, bs) = f
    funArgs (f, args, bs) = args

    lookupFun f = 
      case [(d, i) | (d, i) <- zip p [0..], funName d == f]  of
              [] -> error ("RedCompile.hs: undefined function " ++ f)
              [(d, i)] -> (d, i)
              other -> error ("RedCompile.hs: function multiply defined " ++ f)
