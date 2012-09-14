module ZeroInstall.Store where

import Prelude hiding (lookup)
import qualified System.Posix.User as User
import qualified System.Environment.XDG.BaseDir as Basedir
import System.Directory (doesDirectoryExist)
import System.FilePath (joinPath)
import Data.Maybe (catMaybes)
import Control.Monad (liftM)
import Control.Error (headMay, note)
import ZeroInstall.Model (Digest, formatDigest)

data Store = Store FilePath

getStores :: IO [Store]
getStores = do
	user <- userStoreDir
	sys <- systemStoreDirs
	return $ map Store (user:sys)

firstDefined :: [Maybe a] -> Maybe a
firstDefined = headMay . catMaybes

lookupAny :: [Store] -> [Digest] -> IO (Either String FilePath)
lookupAny stores digests = liftM (note errorMessage) found where
	found = liftM firstDefined $ mapM (lookup stores) digests
	errorMessage =
		("Couldn't locate any of the following digests: " ++
			(concat $ (map $ ("\n - " ++) . formatDigest) digests))

lookup :: [Store] -> Digest -> IO (Maybe FilePath)
lookup stores digest = liftM firstDefined $ mapM (flip lookup' $ digest) stores where
	lookup' :: Store -> Digest -> IO (Maybe FilePath)
	lookup' (Store path) digest = existingDirectory (joinPath [path, (formatDigest digest)])

existingDirectory :: FilePath -> IO (Maybe FilePath)
existingDirectory p = doesDirectoryExist p >>=
	\exists -> return $ if exists then Just p else Nothing

userStoreDir = do
	user <- User.getEffectiveUserName
	cacheDir <- Basedir.getUserCacheDir user
	return (implDir cacheDir)

systemStoreDirs = do
	return [implDir "/var/cache"]

implDir base = joinPath [base, "0install.net", "implementations"]
