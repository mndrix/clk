import System.Environment
import qualified App.Clk.Command.In as In
import qualified App.Clk.Command.Ls as Ls
import qualified App.Clk.Command.Report as Report

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "You must provide a sub-command name"
        (command:rest) -> commandMain command rest

commandMain :: String -> [String] -> IO ()
commandMain "in" = In.main
commandMain "ls" = Ls.main
commandMain "report" = Report.main
commandMain cmd  = error $ "Unknown command: " ++ cmd
