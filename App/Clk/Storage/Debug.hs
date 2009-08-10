-- A storage module that displays debugging messages each time
-- an operation is called.

module App.Clk.Storage.Debug (StorageDebug, open) where
import App.Clk (Event)
import App.Clk.Storage

data StorageDebug = StorageDebug
instance Storage StorageDebug where
    open d   = do
        putStrLn $ "opening: " ++ show d
        return StorageDebug
    insert _ e = putStrLn $ "inserting:\n" ++ show e
    remove _ e = putStrLn $ "removing: " ++ show e
    find_by_id _ _ = undefined
    find_by_id_prefix _ _ = undefined
    find_between _ a b = undefined
    close = putStrLn $ "closing"
