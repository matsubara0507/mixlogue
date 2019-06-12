module Version where

import           RIO

import           Data.Version (Version)
import qualified Data.Version as Version
import           GitHash

showVersion' :: Version -> String
showVersion' v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , giHash gi
  , "(" ++ show (giCommitCount gi) ++ " commits)"
  ]
  where
    gi = $$tGitInfoCwd
