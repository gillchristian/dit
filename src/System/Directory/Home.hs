module System.Directory.Home
  ( expandTilde,
  )
where

import System.Directory (getHomeDirectory)
import System.FilePath (isPathSeparator, (</>))

-- TODO: do full expand (https://github.com/npj/expand-path/blob/master/src/System/Unix/ExpandPath/Monad.hs)
expandTilde :: FilePath -> IO FilePath
expandTilde "~" = getHomeDirectory
expandTilde ('~' : '/' : rest) = do
  home <- getHomeDirectory
  pure $ home </> rest
expandTilde path = pure path
