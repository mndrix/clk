module App.Clk.Config where
import App.Clk (Entity)
import App.Clk.Storage (Storage, open)
import App.Clk.Storage.Null

-- This user's entity is most likely stored in a configuration file or an
-- environment variable, so we'll need IO to retrieve it.

get_user_entity :: IO Entity
get_user_entity = do
  putStrLn "getting user entity"
  return "michael@ndrix.org"


-- Again, with configuration or an environment variable, the user specifies
-- his preferred method for storing his events.  A typical user may have
-- events stored in multiple storage formats, but the default is the one used
-- when inserting new events.

-- open_default_storage :: Storage a => IO a
open_default_storage :: IO StorageNull
open_default_storage = do
    putStrLn "opening the default storage"
    open "foo"
