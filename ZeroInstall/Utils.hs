module ZeroInstall.Utils where

import Control.Monad (liftM)
import Control.Error (headMay, partitionEithers, fmapL)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist)

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a) = Left (f a)
mapLeft f (Right b) = Right b

mapRight :: (b1 -> b2) -> Either a b1 -> Either a b2
mapRight = liftM

-- return the first `Left` if there were any lefts, else return the list of all rights
collectRight :: [Either a b] -> Either a [b]
collectRight eithers = (headMay lefts) `toLeft` rights
	where
		(lefts, rights) = partitionEithers eithers

toLeft Nothing r = Right r
toLeft (Just x) _ = Left x

toRight Nothing left = Left left
toRight (Just x) _ = Right x


prependErrorMessage :: String -> Either String a -> Either String a
prependErrorMessage m = fmapL (\err -> m ++ err)

appendErrorMessage :: String -> Either String a -> Either String a
appendErrorMessage m = fmapL (\err -> err ++ m)

mkMap :: Ord k => (a -> (k, v)) -> [a] -> Map.Map k v
mkMap toPair elems = Map.fromList $ map toPair elems

requireRight :: Either String a -> IO a
requireRight e = either fail return e


existingDirectory :: FilePath -> IO (Maybe FilePath)
existingDirectory p = doesDirectoryExist p >>=
	\exists -> return $ if exists then Just p else Nothing

