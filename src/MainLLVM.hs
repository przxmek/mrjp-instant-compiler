module Main where

  import System.Environment ( getArgs )

  import CompilerJVM ( compile )
  import MainCommon ( run, showHelp )

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      []          -> showHelp
      ["--help"]  -> showHelp
      [file]      -> do
        txt <- run file compile
        writeFile "llvm.ll" txt
        return ()
      