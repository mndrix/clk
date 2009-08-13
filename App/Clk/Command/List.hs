module App.Clk.Command.List where
import Prelude hiding (tail)
import App.Clk.Storage (Storage, tail, close)
import App.Clk.Config (open_default_storage)

-- This command lists the most recent entries in the timeline.

run argv = do
    putStrLn "running 'list'"
    store <- open_default_storage
    command_list store 3
    close store

command_list :: Storage a => a -> Int -> IO ()
command_list store count = do
    recent <- tail store count
    mapM (putStrLn . show) recent
    return ()
