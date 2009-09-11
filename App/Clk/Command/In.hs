module App.Clk.Command.In where
import App.Clk (Event(..), Subject, Tags)
import App.Clk.Storage (Storage, insert, close)
import App.Clk.Config (get_user_entity, open_default_storage)
import App.Clk.Command.List (command_list)
import App.Clk.Util
import Data.Time (getCurrentTime)
import System.Console.GetOpt
import Data.Maybe (mapMaybe)

data Flag = Subject String | Tag String
options :: [ OptDescr Flag ]
options =
    [
        Option ['s'] ["subject"] (ReqArg Subject "SUBJECT") "event subject"
    ,   Option ['t'] ["tag"]     (ReqArg Tag     "TAG"    ) "event tags"
    ]

isSubject ( Subject _ ) = True
isSubject ( _         ) = False

isTag ( Tag _ ) = True
isTag ( _     ) = False

-- This command creates a new event on a timeline.  It's the main
-- technique for tracking one's time.

run :: [String] -> IO ()
run argv = do
    store <- open_default_storage
    let (args, extra, error) = getOpt Permute options argv
    command_in store (subject args extra) (tags args)
    command_list store 3    -- run 'list' to show context
    close store

-- determine the subject from the command line flags
subject :: [Flag] -> [String] -> Subject
subject flags extra =
    case filter isSubject flags of
        []                -> unwords extra
        ( Subject s : _ ) -> s

-- determine the tags from the command line flags
tags :: [Flag] -> Tags
tags = mapMaybe p
    where p ( Tag t ) = Just t
          p _         = Nothing

-- This is a rough outline for how the 'clk in' command might be implemented.
-- Presumably there's something at a higher level that parses the command line
-- and ascertains the subject and tags the user wants.

command_in :: Storage a => a -> Subject -> Tags -> IO ()
command_in store subject tags = do
    t <- getCurrentTime
    e <- get_user_entity
    let event = Event e t subject tags
    output <- run_hook "pre-in" [] (show event)
    insert store $ read $ head $ lines output
    return ()
