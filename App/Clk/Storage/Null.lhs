A storage module that does absolutely nothing.  It's useful for
testing.

> module App.Clk.Storage.Null where
> import App.Clk
> type App.Clk.Storage.Null = ()
> instance Storage App.Clk.Storage.Null where
>   open   _              = return ()
>   insert _              = return ()
>   remove _              = return ()
>   find_by_id _ _        = return None
>   find_by_id_prefix _ _ = return None
>   find_between _ _ _    = return []
