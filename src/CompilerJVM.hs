module CompilerJVM where

import Control.Monad.State
import Data.Map as Map

import Parser.AbsInstant


type Result     = StateT Env IO (StackSize, ProgramTxt)
type Env        = Map Ident Val
type Val        = Int
type StackSize  = Int
type ProgramTxt = String

data Instr = BinInstr
data BinInstr = IAdd | ISub | IMul | IDiv deriving (Eq, Ord, Show, Read)
-- data BinaryOp = OpAdd | OpSub | OpMul | OpDiv deriving (Eq, Ord, Show, Read)


emptyProgram :: (Int, String)
emptyProgram = (0, "")


compile :: Program -> IO String
compile program = do
  ((stack, mainTxt), env) <- runStateT (transProgram program) Map.empty
  let locals = (+1) $ Map.size env
  let name = "Program" in return (
    ".class  public " ++ name ++ "\n" ++
    ".super  java/lang/Object\n\n" ++
    ".method public <init>()V\n" ++
    "  aload_0\n  invokespecial java/lang/Object/<init>()V\n  return\n" ++
    ".end method\n\n" ++
    ".method public static main([Ljava/lang/String;)V\n" ++
    ".limit stack "  ++ show stack ++ "\n" ++ 
    ".limit locals " ++ show locals ++ "\n" ++
    mainTxt ++ "  return\n.end method\n")




transProgram :: Program -> Result
transProgram (Prog stmts) =  do
  results <- mapM transStmt stmts
  let programConcat (accStack, accTxt) (stack, txt) = (max accStack stack, accTxt ++ txt)
  let (stack, text) = Prelude.foldl programConcat emptyProgram results
  return (stack, text)


transStmt :: Stmt -> Result
transStmt (SAss ident expr)  = do
  (exprSt, exprTxt) <- transExp expr
  env <- get
  loc <- case Map.lookup ident env of
    Just loc -> return loc
    Nothing -> do
      let loc = Map.size env
      put $ Map.insert ident loc env 
      return loc
  return (exprSt, exprTxt ++ "  istore " ++ show loc ++ "\n")


transStmt (SExp expr)        = do
  (stack, txt) <- transExp expr
  return (stack + 1, txt ++ jvmPrint) where 
    jvmPrint = "  getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
               "  swap\n" ++
               "  invokevirtual java/io/PrintStream/println(I)V\n"


transExp :: Exp -> Result
transExp (ExpAdd exp1 exp2)  = transBinaryOp IAdd exp1 exp2
transExp (ExpSub exp1 exp2)  = transBinaryOp ISub exp1 exp2
transExp (ExpMul exp1 exp2)  = transBinaryOp IMul exp1 exp2
transExp (ExpDiv exp1 exp2)  = transBinaryOp IDiv exp1 exp2
transExp (ExpLit num)        = transIConst num
transExp (ExpVar ident)      = do
  env <- get
  let loc = env Map.! ident
  return (1, "  iload " ++ show loc ++ "\n")



transBinaryOp :: BinInstr -> Exp -> Exp -> Result
transBinaryOp op exp1 exp2  = do
  (e1St, e1Txt) <- transExp exp1
  (e2St, e2Txt) <- transExp exp2
  (   _, opTxt) <- jvmInstr op
  -- let rSt = max e1St e2St
  return (e1St + e2St, e1Txt ++ e2Txt ++ opTxt)


transIConst :: Integer -> Result
transIConst num 
  | num ==     -1                 = wrapInstr1 "iconst_m1"
  | num >=      0 && num <     6  = wrapInstr1 $ "iconst_" ++ show num
  | num >=   -128 && num <   128  = wrapInstr1 $ "bipush " ++ show num
  | num >= -32768 && num < 32768  = wrapInstr1 $ "sipush " ++ show num
  | otherwise                     = wrapInstr1 $ "ldc "    ++ show num




jvmInstr :: BinInstr -> Result
jvmInstr IAdd = wrapInstr 0 "iadd"
jvmInstr ISub = wrapInstr 0 "isub"
jvmInstr IMul = wrapInstr 0 "imul"
jvmInstr IDiv = wrapInstr 0 "idiv"


-- op2Instr :: BinaryOp -> Result
-- op2Instr OpAdd  = wrapInstr0 "iadd"
-- op2Instr OpSub  = wrapInstr0 "isub"
-- op2Instr OpMul  = wrapInstr0 "imul"
-- op2Instr OpDiv  = wrapInstr0 "idiv"




wrapInstr :: Int -> String -> Result
wrapInstr s instr  = return (s, "  " ++ instr ++ "\n")

wrapInstr0 :: String -> Result
wrapInstr0 = wrapInstr 0

wrapInstr1 :: String -> Result
wrapInstr1  = wrapInstr 1
