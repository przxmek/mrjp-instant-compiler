module CompilerJVM where

import Control.Monad.State
import Data.Map as Map

import Parser.AbsInstant




type Result     = StateT Env IO (StackSize, ProgramTxt)
type Env        = Map Ident Location
type Location   = Int
type StackSize  = Int
type ProgramTxt = String

data Instr 
    = IConstM1 | IConst_ Integer | BiPush Integer | SiPush Integer | Ldc Integer 
    | ILoad Location | IStore Location
    | BiI BinaryInstr 
    | PrintStreamISwap
  deriving (Eq, Ord, Show, Read)

data BinaryInstr = IAdd | ISub | IMul | IDiv 
  deriving (Eq, Ord, Show, Read)




emptyResult :: (Int, String)
emptyResult = (0, "")


compile :: Program -> String -> IO String
compile program fileName = do
  ((stack, mainTxt), env) <- runStateT (transProgram program) Map.empty
  let locals = (+1) $ Map.size env
  return (
    ".class public " ++ fileName ++ "\n" ++
    ".super java/lang/Object\n\n" ++
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
  let (stack, text) = Prelude.foldl programConcat emptyResult results
  return (stack, text)


transStmt :: Stmt -> Result
transStmt (SAss ident expr)  = do
  (exprStack, exprTxt) <- transExp expr
  env <- get
  loc <- case Map.lookup ident env of
    Just loc -> return loc
    Nothing -> do
      let loc = Map.size env
      put $ Map.insert ident loc env 
      return loc
  (_, assTxt) <- jvmInstr $ IStore loc

  return (exprStack, exprTxt ++ assTxt)


transStmt (SExp expr)        = do
  (stack, txt)  <- transExp expr
  (printStack, printTxt) <- jvmInstr PrintStreamISwap
  return (max stack printStack, txt ++ printTxt)


transExp :: Exp -> Result
transExp (ExpAdd exp1 exp2)  = transBinaryOp (BiI IAdd) exp1 exp2
transExp (ExpSub exp1 exp2)  = transBinaryOp (BiI ISub) exp1 exp2
transExp (ExpMul exp1 exp2)  = transBinaryOp (BiI IMul) exp1 exp2
transExp (ExpDiv exp1 exp2)  = transBinaryOp (BiI IDiv) exp1 exp2
transExp (ExpLit num)        = transIConst num
transExp (ExpVar ident)      = do
  env <- get
  case Map.lookup ident env of
    (Just loc) -> jvmInstr $ ILoad loc
    Nothing  -> let (Ident var) = ident in
      error $ "Error: undefined variable `" ++ var ++ "`"


transIConst :: Integer -> Result
transIConst num 
  | num ==     -1                 = jvmInstr   IConstM1
  | num >=      0 && num <     6  = jvmInstr $ IConst_ num
  | num >=   -128 && num <   128  = jvmInstr $ BiPush num
  | num >= -32768 && num < 32768  = jvmInstr $ SiPush num
  | otherwise                     = jvmInstr $ Ldc num


transBinaryOp :: Instr -> Exp -> Exp -> Result
transBinaryOp op@(BiI biOp) exp1 exp2  = do
  (e1St, e1Txt) <- transExp exp1
  (e2St, e2Txt) <- transExp exp2
  (   _, opTxt) <- jvmInstr op
  if e1St < e2St
    then return (e2St, e2Txt ++ e1Txt ++ swapBiOp biOp ++ opTxt)
    else return (max e1St $ e2St + 1, e1Txt ++ e2Txt ++ opTxt)
transBinaryOp _ _ _ = return emptyResult


swapBiOp :: BinaryInstr -> String
swapBiOp IAdd = formatInstr "swap"
swapBiOp IMul = formatInstr "swap"
swapBiOp    _ = ""


jvmInstr :: Instr -> Result
jvmInstr  (BiI IAdd) = return (0, formatInstr   "iadd")
jvmInstr  (BiI ISub) = return (0, formatInstr   "isub")
jvmInstr  (BiI IMul) = return (0, formatInstr   "imul")
jvmInstr  (BiI IDiv) = return (0, formatInstr   "idiv")
jvmInstr   IConstM1  = return (1, formatInstr   "iconst_m1")
jvmInstr (IConst_ i) = return (1, formatInstr $ "iconst_" ++ show i)
jvmInstr  (BiPush i) = return (1, formatInstr $ "bipush " ++ show i)
jvmInstr  (SiPush i) = return (1, formatInstr $ "sipush " ++ show i)
jvmInstr     (Ldc i) = return (1, formatInstr $    "ldc " ++ show i)
jvmInstr   (ILoad l) = jvmInstrILoad l
jvmInstr  (IStore l) = jvmInstrIStore l

jvmInstr PrintStreamISwap = return
  (1, "  getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
      "  swap\n" ++
      "  invokevirtual java/io/PrintStream/println(I)V\n")


jvmInstrILoad :: Int -> Result
jvmInstrILoad l
  | l < 4     = return (1, formatInstr $  "iload_" ++ show l)
  | otherwise = return (1, formatInstr $  "iload " ++ show l)


jvmInstrIStore :: Int -> Result
jvmInstrIStore l
  | l < 4     = return (1, formatInstr $  "istore_" ++ show l)
  | otherwise = return (1, formatInstr $  "istore " ++ show l)


formatInstr :: String -> String
formatInstr   instr  = "  " ++ instr ++ "\n"
