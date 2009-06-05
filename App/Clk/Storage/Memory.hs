-- A storage module that keeps everything in memory
-- It provides no persistence

module App.Clk.Storage.Memory where
import App.Clk (Event)
import App.Clk.Storage

data StorageMemory = StorageMemory [Event]
instance Storage StorageMemory where
    insert _ _            = putStrLn "inserting"
    remove _ _            = putStrLn "removing"
    find_by_id _ _        = return Nothing
    find_by_id_prefix _ _ = return Nothing
    find_between _ _ _    = return []

empty = StorageMemory []
