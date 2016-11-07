module Main where

  import System.Environment ( getArgs )

  import CompilerLLVM ( compile )
  import MainCommon ( run, showHelp )


  main :: IO ()
  main = do
    args <- getArgs
    case args of
      []          -> showHelp
      ["--help"]  -> showHelp
      [file]      -> do
        txt <- run file compile
        putStr txt
        writeFile "prog.ll" txt
        return ()
