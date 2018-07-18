{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Docker.Client.Utils where

import qualified Codec.Archive.Tar           as Tar
import qualified Codec.Compression.GZip      as GZip
import           Control.Monad               (filterM, liftM, unless)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy        as BS
import           Data.Monoid                 ((<>))
import           Data.Maybe
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           System.Directory            (doesDirectoryExist, doesFileExist,
                                              getTemporaryDirectory)
import           System.FilePath             (isAbsolute, makeRelative, (</>))
import           System.FilePath.Find        (FilterPredicate,
                                              RecursionPredicate, always,
                                              fileName, find, (==?))
import           System.FilePath.GlobPattern ((~~))
import           System.IO.Error             (tryIOError)
-- import           System.IO.Temp              (withSystemTempDirectory)
--
import           Docker.Client.Http

type File = FilePath
data DirTree = DirTree [File] [DirTree]


data ClusionPattern = ExclusionPattern T.Text |
                      InclusionPattern T.Text deriving (Eq, Show)

data DockerIgnore = DockerIgnore { clusionPatterns :: [ClusionPattern]
                                 } deriving (Eq, Show)

newtype BuildContextRootDir = BuildContextRootDir FilePath deriving (Eq, Show)

makeBuildContext :: forall m. MonadIO m => BuildContextRootDir -> m (Either DockerError FilePath)
makeBuildContext base = liftIO $ tryIOError (makeBuildContext' base) >>= \res -> case res of
    Left e -> return $ Left $ DockerClientError (T.pack $ show e)
    Right c -> return $ Right c

makeBuildContext' :: forall m. MonadIO m => BuildContextRootDir -> m FilePath
makeBuildContext' (BuildContextRootDir base) = do
    uuid <- liftIO UUID.nextRandom
    fs <- liftIO $ getBuildContext $ BuildContextRootDir base
    let relFs = map (makeRelative base) fs
    tmpDir <- liftIO getTemporaryDirectory
    let tmpF = tmpDir </> "docker.context-" <> UUID.toString uuid  <> ".tar.gz"
    liftIO $ BS.writeFile tmpF . GZip.compress . Tar.write =<< Tar.pack base relFs
    return tmpF

toClusionPattern :: T.Text -> ClusionPattern
toClusionPattern p = if T.isPrefixOf "!" p then InclusionPattern p else ExclusionPattern p

parseDockerIgnoreFile :: T.Text -> DockerIgnore
parseDockerIgnoreFile c = DockerIgnore{ clusionPatterns=parseClusions }
    where lines = filter (not . T.isPrefixOf "#") (T.lines c) -- Ignore comments
          parseClusions = map toClusionPattern $ filter (\l -> (l /= "")) lines

getBuildContext :: BuildContextRootDir -> IO [FilePath]
getBuildContext (BuildContextRootDir base) = do
    -- The base dir needs to be a path to a directory and not a path to
    -- a file
    exists <- doesDirectoryExist base
    let abs = isAbsolute base
    unless (exists && abs) $ fail "Path to context needs to be a directory that: exists, is readable, and is an absolute path."
    di <- find always (fileName ==? ".dockerignore") base
    dockerignore <- case di of
        [] -> return $ DockerIgnore []
        -- This should not return more than one result though
        (x:_) -> do
            c <- TIO.readFile x
            return $ parseDockerIgnoreFile c
    -- This will traverse the directory recursively
    fs <- find (shouldRecurse dockerignore) (shouldInclude dockerignore) base
    -- fs is a list of directories *and* files in those directories. So
    -- an example result would look like ["/tmp/project/files",
    -- "/tmp/project/files/file1.txt"] and we want just the individual
    -- files otherwise tar duplicates them when making the archive.
    fs' <- filterM doesFileExist fs
    -- For some reason base is in there as well and we don't need that
    return $ filter (not . (==) base) fs'

shouldInclude :: DockerIgnore -> FilterPredicate
shouldInclude d = check `liftM` fileName
    where check f = clusionCheck f (clusionPatterns d)

shouldRecurse :: DockerIgnore -> RecursionPredicate
shouldRecurse d = check `liftM` fileName
    where check f = clusionCheck f (clusionPatterns d)

oneClusionCheck :: FilePath -> ClusionPattern -> Maybe Bool
oneClusionCheck f (ExclusionPattern p) = if f ~~ T.unpack p then Just False else Nothing
oneClusionCheck f (InclusionPattern p) = if f ~~ T.unpack p then Just True else Nothing

clusionCheck :: FilePath -> [ClusionPattern] -> Bool
clusionCheck f ps = case mapMaybe (oneClusionCheck f) (reverse ps) of
    [] -> True  -- default to include
    (clude:_) -> clude
