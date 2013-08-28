module Flite.RedFrontend (frontend) where

import Flite.LambdaLift
import Flite.Syntax
import Flite.Traversals
import Flite.ConcatApp
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Strictify
import Flite.Inline
import Flite.Fresh
import Flite.WorkerWrapper
import Control.Monad
import Flite.Pretty

frontend :: Prog -> Prog
frontend p =
  snd (runFresh (frontendM p) "$" 0)

frontendM :: Prog -> Fresh Prog
frontendM p =
  do p0 <- desugarCase (identifyFuncs p) >>= desugarEqn
     p1 <- inlineLinearLet (concatApps p0)
             >>= inlineSimpleLet
             >>= return . lambdaLift 'A'
             >>= return . concatApps
             >>= return . caseElim--WithCaseStack  --caseElim
             >>= return . concatApps
            -- >>= return . strictifyPrim
             >>= return . strictifyPrimOld   --
             >>= return . concatApps         --
     return p1
