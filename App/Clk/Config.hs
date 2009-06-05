module App.Clk.Config where
import App.Clk (Entity)
import App.Clk.Storage (Storage)
import App.Clk.Storage.Null (StorageNull, empty)

-- This user's entity is most likely stored in a configuration file or an
-- environment variable, so we'll need IO to retrieve it.

get_user_entity :: IO Entity
get_user_entity = do
  putStrLn "getting user entity"
  return "michael@ndrix.org"

open_default_storage :: IO StorageNull
open_default_storage = return App.Clk.Storage.Null.empty
