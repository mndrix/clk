The main clk program just dispatches to the specific functions that
implement each clk subcommand.

> import System.Environment (getArgs)

A list of command modules.  The dispatch function uses these modules
to implement the commands.  dispatch is just a list of commands that
we support

> import App.Clk.Command.In
> dispatch "in"    a = App.Clk.Command.In.run a
> dispatch command _ = do
>   putStrLn $ "There's no command named '" ++ command ++ "'"

A user must specify a command that he wants to run.  If he doesn't, we
should probably run the 'help' command or something.  For now, we'll
just produce an error.

> main = do
>   args <- getArgs
>   case args of
>       []                 -> putStrLn "error: You must specify a command"
>       command:arguments  -> dispatch command arguments
