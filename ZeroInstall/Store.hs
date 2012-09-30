module ZeroInstall.Store where

import Prelude hiding (lookup)
import qualified System.Environment.XDG.BaseDir as Basedir
import System.Directory (doesDirectoryExist)
import System.FilePath (joinPath)
import Data.Maybe (catMaybes)
import Control.Monad (liftM)
import Control.Error (headMay, note)
import ZeroInstall.Model (Digest, formatDigest)
import ZeroInstall.Utils
import qualified ZeroInstall.Namespaces as NS

data Store = Store FilePath

getStores :: IO [Store]
getStores = do
	user <- userStoreDir
	sys <- systemStoreDirs
	return $ map Store (user:sys)

firstDefined :: [Maybe a] -> Maybe a
firstDefined = headMay . catMaybes

bulletList :: [String] -> String
bulletList items = (concat $ map ("\n - " ++) items)

lookupAny :: [Store] -> [Digest] -> IO (Either String FilePath)
lookupAny stores digests = liftM (note errorMessage) found where
	found = liftM firstDefined $ mapM (lookup stores) digests
	errorMessage =
		("Couldn't locate any of the following digests: " ++
			(bulletList $ map formatDigest digests) ++
			"\nIn the following implementation stores:" ++
			(bulletList $ map getPath stores)
		)

getPath (Store path) = path

lookup :: [Store] -> Digest -> IO (Maybe FilePath)
lookup stores digest = liftM firstDefined $ mapM (flip lookup' $ digest) stores where
	lookup' :: Store -> Digest -> IO (Maybe FilePath)
	lookup' (Store path) digest = existingDirectory (joinPath [path, (formatDigest digest)])

userStoreDir = Basedir.getUserCacheDir cachePath

systemStoreDirs = do
	return $ [joinPath ["/var/cache", cachePath]]

cachePath = joinPath [NS.configSite, "implementations"]
