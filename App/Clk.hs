module App.Clk where
import Data.Time (UTCTime)

data Event  = Event Entity Time Subject Tags

-- In the clk data model, an Event is the most fundamental data structure.
-- All else is inferred from these.

type Entity = String

-- Any string identifying the thing whose time is being tracked.  A
-- decent choice is the user's email address.  If tracking the amount of
-- time a room was occupied (or something), the room's floor and room
-- number could be used.

type Time   = UTCTime

-- The precise time when this event occurred.

type Subject= String

-- A one line description of this event.  This is intended to be similar
-- to an email's subject line

type Tag    = String
type Tags   = [Tag]

-- These are the classic folksonomy style tags.  Each tag should be about
-- a couple words long and classifies the event into arbitrary, possibly
-- overlapping, categories.


-- Some functions over the Event type

type EventID = String


identity    :: Event -> EventID
identity e = undefined

-- A unique identifier for this event.  It's probably a hash of the Event
-- parts in some canonical representation.  The canonical representation
-- should be easily calculated by any programming language.  Perhaps the
-- SHA-1 hash of a tab separated list of entity, time, subject and tags.
-- The strings should be considered lists of bytes so that we don't have
-- to choose a preferred encoding.

entity :: Event -> Entity
entity e = undefined
time :: Event -> Time
time e = undefined
subject :: Event -> Subject
subject e = undefined
tags :: Event -> Tags
tags e = undefined

-- Accessors for the parts of an event.

set_time    :: Event -> Time -> Event
set_time e t = undefined

-- Create a new Event from an existing event by changing the time.  This will be
-- useful when a user wants to resume work on an event that was started earlier.
-- For instance "clk in e32ec5"


-- This is a rough outline for how the 'clk ls' command might be
-- implemented.  Again, there's probably some code higher up which
-- parses user's input to create these arguments.

-- command_ls :: Time -> Time -> IO ()
-- command_ls start end = do
--     store <- open_default_storage
--     events <- find_by_time store start end
--     putStr $ events_to_lines events
