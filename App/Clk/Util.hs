module App.Clk.Util where

import System.Environment

getClkDir :: IO String
getClkDir = do
    home <- getEnv "HOME"
    return $ home ++ "/.clkq/"  -- q avoids collision with older data

