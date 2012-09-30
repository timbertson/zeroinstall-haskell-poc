{-# LANGUAGE ScopedTypeVariables #-}
module ZeroInstall.App where

import System.FilePath
import qualified System.Environment.XDG.BaseDir as Basedir
import qualified ZeroInstall.Namespaces as NS
import Control.Error
import Text.Regex (matchRegex, mkRegex)
import qualified ZeroInstall.Model as Model
import qualified ZeroInstall.Selections as Selections
import ZeroInstall.Run (runSelections)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT)
import System.Directory (doesDirectoryExist)

data App = App {
	config :: Config
	,path :: FilePath
	-- ,lastChecked :: DateTime
	} deriving Show

data Config = Config {} deriving Show

todo = error "TODO"

updateLastChecked :: App -> IO ()
updateLastChecked _ = todo

setSelections :: App -> Model.Selections -> App
setSelections _ _ = todo

destroy :: App -> IO ()
destroy _ = todo

getName :: App -> String
getName _ = todo

validName str = isJust $ matchRegex (mkRegex "^[^./\\:=;'\"][^/\\:=;'\"]*$") str

validateName :: String -> Either String String
validateName name = if (validName name) then (Right name) else (Left ("Invalid application name " ++ name))

lookupApp :: Config -> String -> IO (Either String App)
lookupApp config name = runEitherT $ do
	name <- liftEither $ validateName name
	possibleDirs <- lift $ (Basedir.getAllConfigDirs (joinPath [NS.configSite, "apps", name]))
	maybeDir <- lift $ headFilterM doesDirectoryExist possibleDirs
	appDir <- liftEither $ note ("No such application " ++ name) maybeDir
	return $ App {
		config = config
		,path = appDir
		-- ,lastChecked = startOfTime
	}

headFilterM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
headFilterM predicate [] = return Nothing
headFilterM predicate (x:xs) = predicate x >>= \ok -> if ok then return (Just x) else headFilterM predicate xs

runApp :: App -> [String] -> IO ()
runApp app args = do
	let selectionFile = (path app) </> "selections.xml"
	xml <- Selections.loadXml selectionFile
	let maybeSels = Selections.getSelections xml
	sels <- either fail return maybeSels
	runSelections sels args
