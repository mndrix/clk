> module App.Clk where
> import Data.Time (UTCTime)
> import App.Clk.Storage.Null

> data Event  = Event Entity Time Subject Tags

In the clk data model, an Event is the most fundamental data structure.
All else is inferred from these.

> type Entity = String

Any string identifying the thing whose time is being tracked.  A
decent choice is the user's email address.  If tracking the amount of
time a room was occupied (or something), the room's floor and room
number could be used.

> type Time   = UTCTime

The precise time when this event occurred.

> type Subject= String

A one line description of this event.  This is intended to be similar
to an email's subject line

> type Tag    = String
> type Tags   = [Tag]

These are the classic folksonomy style tags.  Each tag should be about
a couple words long and classifies the event into arbitrary, possibly
overlapping, categories.


Some functions over the Event type

> type EventID = String


identity    :: Event -> EventID

A unique identifier for this event.  It's probably a hash of the Event
parts in some canonical representation.  The canonical representation
should be easily calculated by any programming language.  Perhaps the
SHA-1 hash of a tab separated list of entity, time, subject and tags.
The strings should be considered lists of bytes so that we don't have
to choose a preferred encoding.

entity      :: Event -> Entity
time        :: Event -> Time
subject     :: Event -> Subject
tags        :: Event -> Tags

Accessors for the parts of an event.

set_time    :: Event -> Time -> Event

Create a new Event from an existing event by changing the time.  This will be
useful when a user wants to resume work on an event that was started earlier.
For instance "clk in e32ec5"


> type Directory = FilePath

A directory on a file system.


A type class defining methods that storage implementations must provide.

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

> get_user_entity :: IO Entity
> get_user_entity = do
>   putStrLn "getting user entity"
>   return "michael@ndrix.org"

This user's entity is most likely stored in a configuration file or an
environment variable, so we'll need IO to retrieve it.

> open_default_storage :: Storage a => IO a
> open_default_storage = do
>   putStrLn "opening the default storage"
>   App.Clk.Storage.Null.open ""

Again, with configuration or an environment variable, the user specifies
his preferred method for storing his events.  A typical user may have
events stored in multiple storage formats, but the default is the one used
when inserting new events.


This is a rough outline for how the 'clk ls' command might be
implemented.  Again, there's probably some code higher up which
parses user's input to create these arguments.

command_ls :: Time -> Time -> IO ()
command_ls start end = do
    store <- open_default_storage
    events <- find_by_time store start end
    putStr $ events_to_lines events
