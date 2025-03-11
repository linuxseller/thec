import Data.Time
import System.Directory
import System.Process
import System.Exit
import System.Environment

data Level = Info | Warn | Error

nobLog :: Level -> String -> IO ()
nobLog Info = putStrLn.("[ INFO] " <>)
nobLog Warn = putStrLn.("[ WARN] " <>)
nobLog Error = putStrLn.("[ERROR] " <>)

target = "./Main"

main :: IO ()
main = do
  args <- getArgs
  builder_app_src_time <- getModificationTime "nob.hs"
  builder_app_time <- getModificationTime "nob"
  if builder_app_time < builder_app_src_time
    then do
      nobLog Info "Rebuilding Nob"
      handle <- runCommand "ghc nob.hs"
      exitId <- waitForProcess handle
      if exitId /= ExitSuccess
        then do
          nobLog Error "Couldn't rebuild nob"
          exitWith exitId
        else do
          nobLog Info "Built nob sucessfully"
      handle <- runCommand "rm nob.hi nob.o"
      exitId <- waitForProcess handle
      handle <- runCommand $ "./nob " <> unwords args
      exitId <- waitForProcess handle
      exitWith ExitSuccess
    else return ()
  nobLog Info "Building program"
  _ <- createDirectoryIfMissing False "temp"
  handle <- runCommand $ "ghc -isrc src/Main.hs src/Parser.hs src/CompileAst.hs src/Data/IR.hs src/Data/Parser.hs src/Compilers/X86_64linux.hs -o " <> target <> " -outputdir temp"
  exitId <- waitForProcess handle
  nobLog Info $ "Build exited with code " <> show exitId
  if exitId /= ExitSuccess then do
    nobLog Error $ "Couldn't build " <> target
    exitWith exitId
  else do
    nobLog Info $ "Built " <> target <> " sucessfully"
  if "runMain" `elem` args then do
    nobLog Info $ "Running " <> target
    handle <- runCommand target
    exitId <- waitForProcess handle
    if exitId /= ExitSuccess then do
      nobLog Error $ "Couldn't run " <> target
      exitWith exitId
    else do
      nobLog Info $ "Ran " <> target <> "successfully"
    nobLog Info "Resulted assembly:"
    assembly <- readFile "testing/asmest.asm"
    putStrLn assembly
    return ()
  else do return ()
  if "runAsm" `elem` args then do
    nobLog Info "Compiling assembly"
    handle <- runCommand "fasm testing/asmest.asm"
    exitId <- waitForProcess handle
    if exitId /= ExitSuccess then do
      nobLog Error $ "Couldn't build assembly"
      exitWith exitId
    else do
      nobLog Info $ "Built assembly sucessfully"
    handle <- runCommand "ld -o testing/asmtest testing/asmest.o -lc -dynamic-linker /lib64/ld-linux-x86-64.so.2"
    exitId <- waitForProcess handle
    return ()
  else do return ()
  exitWith exitId
