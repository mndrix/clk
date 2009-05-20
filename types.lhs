In the clk data model, an Event is the most fundamental data structure.
All else is inferred from these.
> type Entity = String
> type Time   = Integer
> type Subject= String
> type Tag    = String
> type Tags   = [Tag]
> data Event  = Event Entity Time Subject Tags

Some functions over the Event type
> type EventID = String

This is probably a hash of the Event parts in some canonical representation.
The canonical representation should be easily calculated by any programming
language.  Perhaps the SHA-1 hash of a tab separated list of entity, time,
subject and tags in UTF-8 encoding.  Maybe the encoding can be ignored and
we'll just treat the encoding of those strings as part of the crucial data
that we're hashing.
> id          :: Event -> EventID
> entity      :: Event -> Entity
> time        :: Event -> Time

Create a new Event from an existing event by changing the time.  This will be
useful when a user wants to resume work on an event that was started earlier.
For instance "clk in e32ec5"
> set_time    :: Event -> Time -> Event
> subject     :: Event -> Subject

Should this return the tags in a canonical order?
> tags        :: Event -> Tags

Is there a Haskell standard type that represents a directory on a file system?
> type Directory = String

A type class defining methods that storage implementations must provide.
> class Storage a where

The idea is that almost any technique for storing events will want some place
to locally record state.  A flat-file implementation would use a single file.
A key-value store would need to store its data files.  A database storage
mechanism may not need to store anything (connection info would probably
be in configuration), so it can just ignore the directory.
>     open            :: Directory -> IO a
>     insert          :: a -> Event -> IO ()
>     remove          :: a -> Event -> IO ()
>     find_by_id      :: a -> EventID -> IO Maybe Event
>     find_by_id_prefix :: a -> String -> IO Maybe Event
>     find_between    :: a -> Time -> Time -> IO [Event]

This user's entity is most likely stored in a configuration file or an
environment variable.
> get_user_entity :: IO Entity

Again, with configuration or an environment variable, the user specifies
his preferred method for storing his events.  A typical user may have
events stored in multiple storage formats, but the default is the one used
when inserting new events.
> open_default_storage :: IO Storage


This is a rough outline for how the 'clk in' command might be implemented.
Presumably there's something at a higher level that parses the command line
and ascertains the subject and tags the user wants.
> command_in :: Subject -> Tags -> IO ()
> command_in subject tags = do
>     t <- getCurrentTime
>     e <- get_user_entity
>     store <- open_default_storage
>     insert store ( Event e t subject tags )
>     return
