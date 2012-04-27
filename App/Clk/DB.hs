module App.Clk.DB
    ( DbName(Timeline)
    , addEntry
    , entriesWithin
    , withDB
    ) where

import App.Clk.Entry
    ( Entry(..)
    , isWithin
    , readStore
    , showStore
    , setDuration
    , setDurationNow
    )
import App.Clk.Util
    ( getClkFile
    , iso8601
    , strftime
    , tween
    )
import Data.Period (Period(start))

import Data.Time.Clock (UTCTime, getCurrentTime)

import Database.LevelDB
    ( DB
    , Option(CreateIfMissing)
    , iterSeek
    , iterValue
    , mapIter
    , put
    , withLevelDB
    , withIterator
    )

import qualified Data.ByteString.Char8 as B


data DbName = Timeline

withDB :: DbName -> (DB -> IO a) -> IO a
withDB n f = do
    file <- getClkFile (name n ++ ".leveldb")
    withLevelDB file [CreateIfMissing] f
  where
    name Timeline = "timeline"

key :: UTCTime -> B.ByteString
key = B.pack . strftime iso8601

addEntry :: DB -> Entry -> IO ()
addEntry db entry = do
    put db [] k value
  where
    k = key $ entryTime entry
    value = B.pack $ showStore entry

entriesWithin :: DB -> Period -> IO [Entry]
entriesWithin db p = do
    now <- getCurrentTime
    withIterator db [] $ \iter -> do
        iterSeek iter (key $ start p)
        (within,without) <- span (isWithin p) `fmap` mapIter iterEntry iter
        let front = tween setDuration within
        let final = setDurationNow (last within)
        return $ case (within,without) of
                    ([],_) -> []
                    (_,[]) -> front ++ [final now]
                    (_,e:_) -> front ++ [final $ entryTime e]
  where
    iterEntry iter = (readStore . B.unpack) `fmap` iterValue iter
