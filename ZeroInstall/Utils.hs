module ZeroInstall.Utils where

import Control.Error (headMay, partitionEithers)

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a) = Left (f a)
mapLeft f (Right b) = Right b

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
