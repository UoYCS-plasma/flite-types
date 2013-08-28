module Flite.Flite (main) where

import Flite.Syntax
import Flite.ParseLib
import Flite.Parse
import Flite.Pretty
import Flite.Interp
import Flite.Inline
import Flite.Compile
import Flite.RedCompile
import Flite.TypeChecker2
import qualified Flite.RedFrontend
import Data.List
import System
import System.IO
import System.Console.GetOpt

data Flag =
    Desugar
  | CompileToC
  | CompileToRed
  | Inline (Maybe Int)
  | StrictnessAnalysis
  | TypeCheck
  | CompileType
  deriving Eq

isDisjoint (Inline i) = False
isDisjoint StrictnessAnalysis = False
isDisjoint flag = True

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (NoArg Desugar) "desugar"
  , Option ['c'] [] (NoArg CompileToC) "compile to C"
  , Option ['r'] [] (NoArg CompileToRed) "compile to Reduceron templates"
  , Option ['i'] [] (OptArg (Inline . fmap read) "MAXAPS")
                    "inline small function bodies"
  , Option ['s'] [] (NoArg StrictnessAnalysis) "employ strictness analysis"
  , Option ['t'] [] (NoArg TypeCheck) "type-checking"
  , Option ['R'] [] (NoArg CompileType) "compile to Reduceron + type-info"
  ]

header = "Usage: Flite [OPTION...] FILE.hs"

main =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, [fileName], []) -> run flags fileName
       (_, _, errs) -> error (concat errs ++ usageInfo header options)

run flags fileName =
  do contents <- readFile fileName
     let p = parse prog contents
     let inlineFlag = head $ [InlineAll | Inline Nothing <- flags]
                          ++ [InlineSmall i | Inline (Just i) <- flags]
                          ++ [NoInline]
     case filter isDisjoint flags of
       [] -> interp inlineFlag p `seq` return ()
       [Desugar]       ->
         putStrLn $ pretty $ Flite.RedFrontend.frontend p
       [CompileToC]    -> putStrLn $ compile inlineFlag p
       [CompileToRed]  -> mapM_ print $ redCompile p
       [TypeCheck]     -> print $ tcheck p --putStrLn $ showfuntypes $ tcheck p
       [CompileType]   -> do 
                            --tc <- putStrLn $ showfuntypes $ tcheck p
                            --cr <- mapM_ print $ redCompile p  
                            tc <-print (tcheck p,redCompile p)
                            return ()
       _ -> error (usageInfo header options)

-- Auxiliary

split :: Eq a => a -> [a] -> [[a]]
split x xs =
  case elemIndex x xs of
    Nothing -> [xs]
    Just i -> let (first, rest) = splitAt i xs in
                first : split x (dropWhile (== x) rest)
