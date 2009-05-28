> module App.Clk.Command.In where
> import App.Clk
> import Data.Time (getCurrentTime)

This command creates a new event on a timeline.  It's the main
technique for tracking one's time.

> run _ = do
>   putStrLn "running 'in'"
>   command_in "a subject" ["first", "second"]

This is a rough outline for how the 'clk in' command might be implemented.
Presumably there's something at a higher level that parses the command line
and ascertains the subject and tags the user wants.

> command_in :: Subject -> Tags -> IO ()
> command_in subject tags = do
>     t <- getCurrentTime
>     e <- get_user_entity
>     store <- open_default_storage
>     insert store ( Event e t subject tags )
>     return ()
