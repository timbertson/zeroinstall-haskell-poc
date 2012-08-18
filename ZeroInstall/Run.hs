module Main where

import System.Posix.Env (getEnv,setEnv)
import System.FilePath (joinPath)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import ZeroInstall.Model

debug :: String -> IO ()
debug = putStrLn

doEnvBinding :: Binding -> Maybe FilePath -> IO ()
doEnvBinding binding path = do
	resolveEnvBinding binding path >>= update
	where
		update (Just (name, val)) = setEnv name val True
		update Nothing = return ()

resolveEnvBinding :: Binding -> Maybe FilePath -> IO (Maybe (String, String))
resolveEnvBinding (Binding name binding) maybePath =
	case maybePath of
		Nothing -> debug ("not setting " ++ name ++ " as we selected a package implementation") >> (return Nothing)
		Just path -> getNewValue binding path
	where
		getNewValue (EnvironmentBinding alg value) path = do
			existingVal <- getEnv name
			let expanded = expandBindingVal value path
			return (Just (name, applyBindingAlg alg expanded existingVal))
		getNewValue binding _ = debug ("Unable to handle binding " ++ (show binding)) >> (return Nothing)

defaultValue :: a -> Maybe a -> a
defaultValue v Nothing = v
defaultValue _ (Just v) = v

defaultPathSep = ":" -- TODO: windows

expandBindingVal :: EnvironmentValue -> String -> String
expandBindingVal (EnvValue v) _ = v
expandBindingVal (EnvInsert v) base = joinPath [base, v]

applyBindingAlg :: BindingAlg -> String -> Maybe String -> String
applyBindingAlg (Replace) new _ = new
applyBindingAlg (AddToExisting sep mode) new existing =
	intercalate sep $ case mode of
		Prepend -> new : existingList
		Append -> existingList ++ [new]
	where
		existingList = (maybeToList existing)
