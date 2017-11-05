module Main where

  import System.Environment ( getArgs )
  import System.FilePath ( replaceExtension, takeDirectory )
  import System.Process ( system )

  import CompilerJVM ( compile )
  import MainCommon ( run, showHelp )


  main :: IO ()
  main = do
    args <- getArgs
    case args of
      ["--help"]  -> showHelp
      [file]      -> do
        txt <- run file compile

        let outputFilePath = replaceExtension file ".j"
        writeFile outputFilePath txt
        putStrLn $ "Generated: " ++ outputFilePath
        system $ "java -jar lib/jasmin.jar " ++ outputFilePath ++ " -d " ++ takeDirectory outputFilePath

        return ()    
      _           -> showHelp
