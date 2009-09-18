-- A storage module with the least possible thought
module App.Clk.Storage.Naive (StorageNaive, open) where
import App.Clk (Event, isBetween)
import App.Clk.Storage
import Data.Period
import System.IO (Handle, IOMode(..), withFile, hGetContents, hPutStrLn)

data StorageNaive = StorageNaive FilePath
instance Storage StorageNaive where
    open   = open_
    insert (StorageNaive f) e = withFile f AppendMode ( insert_ e )
    remove _ e = putStrLn $ "removing: " ++ show e
    find_by_id _ _ = undefined
    find_by_id_prefix _ _ = undefined
    findBetween (StorageNaive f) p = withFile f ReadMode (findBetween_ p)
    tail (StorageNaive f) c = withFile f ReadMode (tail_ c)
    close _ = return ()

open_ d = return $ StorageNaive (d++"events.log")

insert_ e h = hPutStrLn h (show e)

tail_ count h = do
    content <- hGetContents h
    return $! map read $ end $ lines content
        where end x = drop ( length x - count ) x

findBetween_ :: Period -> Handle -> IO [Event]
findBetween_ p h = do
    content <- hGetContents h
    return $! filter ( isBetween p ) $ map read $ lines content
