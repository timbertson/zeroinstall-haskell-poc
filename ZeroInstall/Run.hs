#!/usr/bin/env runghc
module Main where

import System.Posix.Env (getEnv,setEnv)
import System.Cmd (rawSystem)
import System.FilePath (joinPath)
import Control.Monad (liftM)
import Data.List (intercalate, find)
import Data.Maybe (maybeToList)
import ZeroInstall.Model
import qualified ZeroInstall.Selections as Selections

debug :: String -> IO ()
debug = putStrLn

doEnvBinding :: Maybe FilePath -> Binding -> IO ()
doEnvBinding path binding = do
	putStrLn (show binding)
	resolveEnvBinding binding path >>= update
	where
		update (Just (name, val)) = putStrLn ("setting $" ++ name ++ "=" ++ val) >> setEnv name val True
		update Nothing = return ()

resolveEnvBinding :: Binding -> Maybe FilePath -> IO (Maybe (String, String))
resolveEnvBinding (Binding name bindingValue) maybePath =
	case maybePath of
		Nothing -> debug ("not setting " ++ name ++ " as we selected a package implementation") >> (return Nothing)
		Just path -> do
			existingVal <- getEnv name
			return $ Just (name, getNewValue bindingValue existingVal path)
	where
		getNewValue :: BindingValue -> Maybe String -> FilePath -> String
		getNewValue (EnvironmentBinding alg value sep) existingVal path =
			applyBindingAlg alg expanded sep existingVal where
				expanded = expandBindingVal value path

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
	sel <- getMaybe ("missing \"interface\" on selections") $ find ((== iface) . interface) sels
	let command = selectedCommand >>= \_ -> find ((== selectedCommand) . commandName) (commands sel)
	putStrLn $ "num bundings: " ++ (show $ liftM (length . bindings) sels)
	mapM_ applySelectionBindings sels
	return ()
	where
		applySelectionBindings selection = mapM_ (doEnvBinding (Just "/tmp/TODO")) (bindings selection)

getMaybe :: String -> Maybe a -> IO a
getMaybe message m = maybe (fail message) return m

main = do
	xml <- Selections.loadXml "sels.xml"
	let maybeSels = Selections.getSelections xml
	sels <- either fail return maybeSels
	runSelections sels
