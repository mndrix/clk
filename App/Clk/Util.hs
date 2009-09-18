module App.Clk.Util where
import App.Clk.Config
import System.Process
import System.Posix.Files

-- Runs a hook script
run_hook :: String -> [String] -> String -> IO String
run_hook hook args input = do
    hookPath <- clkPath $ "hooks/" ++ hook
    canExec <- isExecutable hookPath
    if not canExec
        then error $ "Hook '" ++ hookPath ++ "' is not executable"
        else readProcess hookPath args input

isExecutable :: FilePath -> IO Bool
isExecutable p = fileAccess p False False True
