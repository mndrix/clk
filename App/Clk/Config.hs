module App.Clk.Config where
import App.Clk (Entity)
import App.Clk.Storage (Storage)
import App.Clk.Storage.Naive

-- This user's entity is most likely stored in a configuration file or an
-- environment variable, so we'll need IO to retrieve it.

get_user_entity :: IO Entity
get_user_entity = do
  return "michael@ndrix.com"

open_default_storage :: IO StorageNaive
open_default_storage = do
    path <- clkPath "storage/debug/"
    App.Clk.Storage.Naive.open path

clkPath :: String -> IO FilePath
clkPath p = return $ "/Users/michael/src/clk3/_clk/" ++ p
