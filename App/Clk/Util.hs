module App.Clk.Util where
import App.Clk.Config
import System.Process

-- Runs a hook script
run_hook :: String -> [String] -> String -> IO String
run_hook hook args input = do
    hookPath <- clkPath $ "hooks/" ++ hook
    readProcess hookPath args input
