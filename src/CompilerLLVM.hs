module CompilerLLVM where

import Control.Monad.State
import Data.Map as Map
import GHC.Err ( error )

import Parser.AbsInstant




type Result     = StateT Env IO ProgramTxt
type Env        = Map Ident Location
type Location   = Int
type ProgramTxt = String




notImplError x = error $ "NOT YET IMPLEMENTED: " ++ show x


compile :: Program -> IO String
compile program = do
  -- let name = "Program"
  runStateT (transProgram program) Map.empty
  return ""





transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> notImplError x 
transProgram :: Program -> Result
transProgram x = case x of
  Prog stmts -> notImplError x
transStmt :: Stmt -> Result
transStmt x = case x of
  SAss ident exp -> notImplError x
  SExp exp -> notImplError x
transExp :: Exp -> Result
transExp x = case x of
  ExpAdd exp1 exp2 -> notImplError x
  ExpSub exp1 exp2 -> notImplError x
  ExpMul exp1 exp2 -> notImplError x
  ExpDiv exp1 exp2 -> notImplError x
  ExpLit integer -> notImplError x
  ExpVar ident -> notImplError x
