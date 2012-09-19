#!/usr/bin/env runghc
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Posix.Env (getEnv,setEnv)
import System.Cmd (rawSystem)
import Debug.Trace
import System.FilePath (joinPath)
import Control.Monad (liftM)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Error (hush, liftEither, fmapL)
import Data.List (intercalate, find)
import Data.Maybe (maybeToList, catMaybes)
import qualified Data.Map as Map
import Data.Map ((!))
import ZeroInstall.Model
import ZeroInstall.Utils
import ZeroInstall.Store (Store, getStores, lookupAny)
import qualified ZeroInstall.Selections as Selections

debug :: String -> IO ()
debug = putStrLn

defaultValue :: a -> Maybe a -> a
defaultValue v Nothing = v
defaultValue _ (Just v) = v

defaultSep = ":" -- TODO: windows

expandBindingVal :: EnvironmentValue -> String -> String
expandBindingVal (EnvValue v) _ = v
expandBindingVal (EnvInsert v) base = joinPath [base, v]

applyBindingAlg :: BindingAlg -> String -> Maybe String -> Maybe String -> String
applyBindingAlg (Replace) new _ _ = new
applyBindingAlg (AddToExisting mode) new sep existing =
	intercalate sep' $ case mode of
		Prepend -> new : existingList
		Append -> existingList ++ [new]
	where
		existingList = (maybeToList existing)
		sep' = maybe defaultSep id sep

runSelections :: Selections -> IO ()
runSelections (Selections iface selectedCommand sels) = do
	selection <-
		requireJust ("cannot find interface " ++ iface ++ " in selections") $
		find ((== iface) . interface) sels
	-- print selection
	-- print (commands selection)
	-- print selectedCommand
	command <-
		requireJust ("cannot find \"" ++ selectedCommand ++ "\" command for interface " ++ iface) $
		find ((== selectedCommand) . commandName) (commands selection)
	-- putStrLn $ "num bundings: " ++ (show $ liftM (length . bindings) sels)
	annotatedSelections :: [(Selection, Maybe FilePath)] <- runEitherT (annotateSelectionPaths sels) >>= requireRight
	let allBindings = getAllBindings sels :: [(Interface, Binding)]
	let applicableBindings = correlateBindings annotatedSelections allBindings :: [(FilePath, Binding)]
	applyBindings applicableBindings
	print command
	return ()

correlateBindings :: [(Selection, Maybe FilePath)] -> [(Interface, Binding)] -> [(FilePath, Binding)]
correlateBindings selectionPathPairs interfaceBindingPairs = catMaybes $ map resolveBinding interfaceBindingPairs where
	selectionPaths :: Map.Map Interface (Maybe FilePath)
	selectionPaths = Map.fromList $ map (\(sel, path) -> (interface sel, path)) selectionPathPairs
	resolveBinding (iface, binding) = case (selectionPaths ! iface) of
		(Just path) -> Just (path, binding)
		Nothing -> trace ("Discarding binding for " ++ iface ++ " - package or optional implementation?") Nothing

applyBindings = mapM_ applyBinding

applyBinding :: (FilePath, Binding) -> IO ()
applyBinding (path, (Binding name (EnvironmentBinding alg value sep))) = do
		existingVal <- getEnv name
		let val = getNewValue existingVal
		putStrLn ("setting $" ++ name ++ "=" ++ val)
		setEnv name val True
	where
		getNewValue :: Maybe String -> String
		getNewValue existingVal =
			applyBindingAlg alg expanded sep existingVal where
				expanded = expandBindingVal value path
--TODO: non-env bindings

getAllBindings :: [Selection] -> [(Interface, Binding)]
getAllBindings sels = concat $ map selectionBindings sels  where
	selectionBindings sel = (collectBindings sel) ++ (collectRequirementBindings sel) ++ (collectCommandBindings sel)

	assoc iface bindings = map ((,) iface) bindings

	collectBindings :: (HasInterface a, HasBindings a) => a -> [(Interface, Binding)]
	collectBindings container = assoc (interface container) (bindings container)

	collectRequirementBindings :: Selection -> [(Interface, Binding)]
	collectRequirementBindings container = concat $ map collectBindings (requirements container)

	collectCommandBindings :: Selection -> [(Interface, Binding)]
	collectCommandBindings sel = concat $ map (\c -> assoc (interface sel) (bindings c)) (commands sel)
	--TODO: do all commands appear in a sels document? or just the ones we have selected?


annotateSelectionPaths :: [Selection] -> EitherT String IO [(Selection, Maybe FilePath)]
annotateSelectionPaths sels = do
	stores <- lift getStores
	paths <- lookupImpls stores sels
	return $ sels `zip` paths
	where
		lookupImpls :: [Store] -> [Selection] -> EitherT String IO [Maybe FilePath]
		lookupImpls stores sels = mapM (EitherT . (lookupImpl stores)) sels

		lookupImpl :: [Store] -> Selection -> IO (Either String (Maybe FilePath))
		lookupImpl stores sel = liftM (prependErrorMessage ("Unable to resolve interface " ++ (interface sel) ++ ":\n"))
			(lookupImpl' stores (implDetails sel))

		lookupImpl' :: [Store] -> ImplementationDetails -> IO (Either String (Maybe FilePath))
		lookupImpl' stores (Local (LocalImplementation _ path))      = return $ Right $ Just path
		lookupImpl' stores (Remote (RemoteImplementation _ digests)) = (liftM . liftM) Just $ lookupAny stores digests
		lookupImpl' stores (Package _)                               = return (Right Nothing)

requireJust :: String -> Maybe a -> IO a
requireJust message m = maybe (fail message) return m

requireRight :: Either String a -> IO a
requireRight e = either fail return e

main = do
	xml <- Selections.loadXml "sels.xml"
	let maybeSels = Selections.getSelections xml
	sels <- either fail return maybeSels
	runSelections sels
