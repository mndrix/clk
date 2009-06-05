module App.Clk.Command.In where
import App.Clk (Event(..), Subject, Tags)
import App.Clk.Storage (Storage, insert)
import App.Clk.Config (get_user_entity, open_default_storage)
import Data.Time (getCurrentTime)
import System.Console.GetOpt

data Flag = Subject String | SecondFlagThatsNeverUsed
options :: [ OptDescr Flag ]
options =
    [
        Option ['s'] ["subject"] (ReqArg Subject "SUBJECT") "event subject"
    ]

-- This command creates a new event on a timeline.  It's the main
-- technique for tracking one's time.

run argv = do
    putStrLn "running 'in'"
    store <- open_default_storage
    let (args, extra, error) = getOpt Permute options argv
    command_in store (subject args) ["first", "second"]

-- determine the subject from the command line flags
subject :: [Flag] -> String
subject [] = ""
subject (  Subject s : _  ) = s
subject (  _         : xs ) = subject xs

-- This is a rough outline for how the 'clk in' command might be implemented.
-- Presumably there's something at a higher level that parses the command line
-- and ascertains the subject and tags the user wants.

command_in :: Storage a => a -> Subject -> Tags -> IO ()
command_in store subject tags = do
    t <- getCurrentTime
    e <- get_user_entity
    insert store ( Event e t subject tags )
    return ()
