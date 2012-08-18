module main where

data App = App {
	config :: Config
	,path :: FilePath
	,lastChecked :: DateTime
	}
		deriving Show

updateLastChecked :: App -> IO ()

setSelections :: App -> Selections -> App

destroy :: App -> IO ()

getName :: App -> String

-- util
getRight Left _ = Nothing
getRight Right v = Just v

validName str = matchRegex (mkRegex "^[^./\\:=;'\"][^/\\:=;'\"]*$")

validateName :: Monad a => Name -> a ()
validateName name = unless (validName name) $ fail ("Invalid application name " ++ name)


-- createApp :: Config -> Name -> Requirements -> IO App
-- createApp config name reqs = do
-- 	validateName name

tryLookupApp :: Config -> Name -> IO (Maybe App)
tryLookupApp = getRight lookupApp'

lookupApp :: Config -> Name -> IO App
lookupApp config name =
	case lookupApp' config name of
		Left err -> throw (SafeException err)
		Right app -> app

lookupApp' :: Config -> Name -> IO (Either String App)
lookupApp' config name = do
	{-
		Get the App for name.
		Returns None if name is not an application (doesn't exist or is not a valid name).
		Since / and : are not valid name characters, it is generally safe to try this
		before calling L{injector.model.canonical_iface_uri}.
	-}
	if (! validateName name)
		then
			return Left ("Invalid application name " ++ name)
		else
			loadApp $ loadFirstConfig [Namespaces.configSite, "apps", name]
	where
		loadApp None = Left ("No such application " ++ name)
		loadApp path = Right $ App {
			config = config
			,path = path
			,lastChecked = startOfTime
		}




validateName :: String -> Bool
