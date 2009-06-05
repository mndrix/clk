A storage module that does absolutely nothing.  It's useful for
testing.

> module App.Clk.Storage.Null where
> import App.Clk.Storage
> data StorageNull = StorageNull
> instance Storage StorageNull where
>   insert _ _            = putStrLn "inserting"
>   remove _ _            = putStrLn "removing"
>   find_by_id _ _        = return Nothing
>   find_by_id_prefix _ _ = return Nothing
>   find_between _ _ _    = return []


> empty = StorageNull
