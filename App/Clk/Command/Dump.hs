module App.Clk.Command.Dump where
import App.Clk.Config (open_default_storage)
import App.Clk.Storage (Storage, close, findBetween)
import Data.List (find)
import Data.Period
import System.Console.GetOpt

data Flag = PeriodFlag String
options :: [ OptDescr Flag ]
options =
    [
        Option "p" ["period"] (ReqArg PeriodFlag "PERIOD") "event date range"
    ]

-- This command prints events to stdout

run :: [String] -> IO ()
run argv = do
    store <- open_default_storage
    let (args, extra, error) = getOpt Permute options argv
    period <- parsePeriod $ maybePeriod args
    command_dump store period
    close store

command_dump :: Storage a => a -> Period -> IO ()
command_dump store period = findBetween store period >>= mapM_ print

maybePeriod :: [Flag] -> String
maybePeriod fs =
    case find isPeriod fs of
        Just (PeriodFlag p) -> p
        Nothing             -> "ever"

isPeriod :: Flag -> Bool
isPeriod _ = True  -- there's only one type of flag at the moment
