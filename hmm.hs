import System.Environment (getArgs)
import System.IO
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Data.Word (Word8)
import Data.ByteString.Internal (w2c)

main = do
    [filename] <- getArgs
    fileTail 10 filename

fileTail :: Int -> FilePath -> IO ()
fileTail n filename = withBinaryFile filename ReadMode (hTail n)

bufferSize :: Integer
bufferSize = 4096

hTail :: Int -> Handle -> IO ()
hTail n h = do
    size <- hFileSize h
    hunks <- mapM (hWindow h) (windows size bufferSize)
    mapM delimited $ combine hunks
    return ()

delimited :: String -> IO ()
delimited = putStr

hWindow :: Handle -> (Integer, Integer) -> IO [String]
hWindow h (start, finish) =
    allocaArray (fromInteger bufferSize) (hWindow_ h start finish)

hWindow_ :: Handle -> Integer -> Integer -> Ptr Word8 -> IO [String]
hWindow_ h start finish ptr = do
    hSeek h AbsoluteSeek start
    rc <- hGetBuf h ptr length
    content <- peekArray length ptr
    return $ reverse $ linesKeep $ map w2c $ take rc content
        where length = fromInteger $ finish - start + 1

windows :: Integer -> Integer -> [(Integer, Integer)]
windows 0 _ = []
windows _ 0 = []
windows totalSize windowSize =
    if   windowSize > totalSize
    then [(0, totalSize)]
    else (start, finish):rest
        where start  = totalSize - windowSize + 1
              finish = totalSize
              rest   = windows (totalSize-windowSize) windowSize


-- combines partial lines into full lines
combine :: [[String]] -> [String]
combine = foldl merge []

merge :: [String] -> [String] -> [String]
merge x [] = x
merge [] x = x
merge xs ys@(y:ys')
    | last y == '\n' = xs ++ ys
    | otherwise      = ( init xs ) ++ ((y++(last xs)):ys')

-- slightly modified from the original 'lines' source
linesKeep :: String -> [String]
linesKeep ""   = []
linesKeep "\n" = ["\n"]
linesKeep s  =
    let (l, s') = breakKeep (=='\n') s in
    (l++"\n") : (linesKeep s')

-- slightly modified from the original 'break' source
breakKeep :: (a -> Bool) -> [a] -> ([a], [a])
breakKeep _ [] = ( [] , [] )
breakKeep p xs@(x:xs')
    | p x       = ( [x], xs' )
    | otherwise = let ( ys, zs ) = breakKeep p xs' in ( x:ys, zs )

-- just like Perl's chomp(), removes a newline character if it's present
chomp :: String -> String
chomp ""     = ""
chomp "\n"   = ""
chomp "\r\n" = ""
chomp (x:xs) = x:(chomp xs)

-- Fóolïnγ around 
