module ZeroInstall.Selections where

import ZeroInstall.Model

import Data.List (partition, intercalate)
import Text.XML.Light as X
import Control.Error
import Control.Monad (liftM)

loadXml :: String -> IO Element
loadXml filename = do
	contents <- readFile filename
	either fail return (parseXml contents)

parseXml :: String -> Either String Element
parseXml str = note "Invalid XML" (X.parseXMLDoc str)

getSelections :: Element -> Either String Selections
getSelections e = do
	selections <- mapM getSelection' (X.findChildren selectionTag  e)
	interface <- getAttr "interface" e
	let commandName = hush $ getAttr "command" e
	return $ Selections interface commandName selections
	where
		getSelection' e = assertElemName selectionTag e >>= getSelection
		selectionTag = ziName "selection"

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a) = Left (f a)
mapLeft f (Right b) = Right b

assertElemName :: QName -> Element -> Either String Element
assertElemName name elem =
	if (name == elName elem)
		then Right elem
		else Left (
			"Expected \"" ++ (qName name) ++ "\" tag, got \"" ++ (tagName elem) ++ "\"" ++ (describeLine elem))

partitionChildren :: (Element -> Bool) -> Element -> ([Element], [Element])
partitionChildren predicate e = partition predicate (X.elChildren e)

getBindings :: Element -> Either String [Binding]
getBindings e = collectRight $ envBindings ++ executableBindings where
	envBindings = map parseEnvironmentBinding $ children "environment"
	executableBindings = map parseExecutableBinding $ children "executable-in-path"
	children tagName = X.findChildren (ziName tagName) e

-- return the first `Left` if there were any lefts, else return the list of all rights
collectRight :: [Either a b] -> Either a [b]
collectRight eithers = (headMay lefts) `toLeft` rights
	where
		(lefts, rights) = partitionEithers eithers

toLeft Nothing r = Right r
toLeft (Just x) _ = Left x

toRight Nothing left = Left left
toRight (Just x) _ = Right x

getOneAttribute :: [(String, String -> a)] -> Element -> Either String a
getOneAttribute specs elem =
	case extracted of
		[match] -> Right $ snd match
		[] -> Left $ "Missing one of " ++ (descAttrs specs) ++ " attributes" ++ (describeLine elem)
		attrs -> Left $ "Only one of " ++ (descAttrs attrs) ++ " attributes allowed" ++ (describeLine elem)
	where
		extracted = catMaybes $ map extract specs
		extract :: (String, String -> a) -> Maybe (String,  a)
		extract (name, proc) = liftM (\val -> (name, proc val)) $ hush (getAttr name elem)
		descAttrs pairs = intercalate ", " $ map (quote . fst) pairs

quote t = "\"" ++ t ++ "\""

parseEnvironmentBinding :: Element -> Either String Binding
parseEnvironmentBinding elem = do
	name <- getAttr "name" elem
	alg <- maybe (Right defaultBindingAlg) lookupBindingAlg $ hush (getAttr "mode" elem)
	liftM (makeBinding name alg) $ getOneAttribute [
		("value", EnvValue),
		("insert", EnvInsert)] elem
	where
		makeBinding :: String -> BindingAlg -> EnvironmentValue -> Binding
		makeBinding name alg val = Binding name (EnvironmentBinding alg val sep)
		sep = hush $ (getAttr "separator" elem)

parseExecutableBinding elem = fail "Not implemented: parseExecutableBinding"

getSelection :: Element -> Either String Selection
getSelection e = do
	id <- getAttr "id" e
	iface <- getAttr "interface" e
	bindings <- return [] -- TODO
	commands <- return [] -- TODO
	let fromFeed = hush $ getAttr "from-feed" e
	impl <- getSelectionImpl e
	return Implementation {
		implId = id
		,implRequirements = []
		,fromFeed = fromFeed
		,interface = iface
		,implBindings = bindings
		,commands = commands
		,implDetails = impl
	}

getSelectionImpl :: Element -> Either String ImplementationDetails
getSelectionImpl e =
	case ((getAttr "package" e), (getAttr "local-path" e)) of
		(Right pkg, _) -> getPackageImpl pkg
		(_, Right localPath) -> getLocalImpl localPath
		_ -> getZiImpl
	where
		getPackageImpl p = do
			dists <- getAttr "distributions" e
			version <- versionText
			return $ Package $ PackageImplementation {
				packageName = p
				,packageVersion = version
				,distributions = words dists
			}

		versionText = getAttr "version" e
		ziVersion = versionText >>= return . parseVersion

		getLocalImpl localPath = do
			version <- ziVersion
			return $ Local $ LocalImplementation version localPath

		getZiImpl = do
			digests <- getChild "manifest-digest" e >>= parseDigests
			version <- ziVersion
			return $ Remote $ RemoteImplementation version digests

nonEmpty :: String -> [a] -> Either String [a]
nonEmpty err [] = Left err
nonEmpty _ val = Right val

parseVersion s = [VersionComponent 1, VersionComponent 0]
parseDigests :: Element -> Either String [Digest]
parseDigests e = nonEmpty "No manifest-digests found" (map toManifest $ elAttribs e)
	where
		toManifest attr = Digest (qName (attrKey attr)) (attrVal attr)

getChild :: String -> Element -> Either String Element
getChild name parent = note errorMessage $ findChild (ziName name) parent
	where
		errorMessage = "Missing \"" ++ name ++ "\" tag in \"" ++ (tagName parent) ++ "\"" ++ (describeLine parent)

getAttr :: String -> Element -> Either String String
getAttr = getAttrNs Nothing

ziNamespace = "http://zero-install.sourceforge.net/2004/injector/interface"
getZiAttr :: String -> Element -> Either String String
getZiAttr = getAttrNs (Just ziNamespace)

getAttrNs :: Maybe String -> String -> Element -> Either String String
getAttrNs uri name elem = note errorMessage $ findAttr qname elem
	where
		qname = QName { qName = name, qURI = uri, qPrefix = Nothing }
		errorMessage =
			"Missing \"" ++ name ++
			"\" attribute in \""++ (tagName elem) ++
			"\" tag" ++ (describeLine elem)

describeLine elem = fromMaybe "" $ liftM describeLine' (elLine elem)
describeLine' num = " (line " ++ (show num) ++ ")"

tagName = qName . elName

ziName :: String -> QName
ziName name = QName { qName = name, qURI = (Just ziNamespace), qPrefix = (Just "zi") }
