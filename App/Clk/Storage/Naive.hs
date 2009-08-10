-- A storage module with the least possible thought
module App.Clk.Storage.Naive (StorageNaive, open) where
import App.Clk (Event)
import App.Clk.Storage
import System.IO (Handle, IOMode(..), openFile, hClose, hPutStrLn)

data StorageNaive = StorageNaive Handle
instance Storage StorageNaive where
    open d   = do
        handle <- openFile (d++"events.log") AppendMode
        return $ StorageNaive handle
    insert (StorageNaive h) e = hPutStrLn h (show e)
    remove _ e = putStrLn $ "removing: " ++ show e
    find_by_id _ _ = undefined
    find_by_id_prefix _ _ = undefined
    find_between _ a b = undefined
    close (StorageNaive h) = hClose h
