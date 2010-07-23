module System.AbsolutePath (
      AbsolutePath
    , mkAbsolutePath
    , takeBaseName
    , takeExtension
    , path
) where
import qualified System.FilePath as FP

data AbsolutePath = AbsolutePath { path :: FP.FilePath }
    deriving (Show, Eq)

mkAbsolutePath :: FP.FilePath -> AbsolutePath
mkAbsolutePath fp
    | FP.isAbsolute fp = AbsolutePath fp
    | otherwise = error "Can't build an AbsolutePath from a relative path"

takeBaseName :: AbsolutePath -> String
takeBaseName = FP.takeBaseName . path

takeExtension :: AbsolutePath -> String
takeExtension = FP.takeExtension . path
