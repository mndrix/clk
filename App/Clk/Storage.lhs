A type class defining methods that storage implementations must provide.

> module App.Clk.Storage where
> import App.Clk

A directory on a file system.

> type Directory = FilePath

> class Storage a where

The idea is that almost any technique for storing events will want some place
to locally record state.  A flat-file implementation would use a single file.
A key-value store would need to store its data files.  A database storage
mechanism may not need to store anything (connection info would probably
be in configuration), so it can just ignore the directory.

>   open              :: Directory -> IO a
>   insert            :: a -> Event -> IO ()
>   remove            :: a -> Event -> IO ()
>   find_by_id        :: a -> EventID -> IO (Maybe Event)
>   find_by_id_prefix :: a -> String -> IO (Maybe Event)
>   find_between      :: a -> Time -> Time -> IO [Event]
