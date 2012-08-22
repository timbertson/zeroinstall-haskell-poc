module Selections where

import ZeroInstall.Model

import Data.List (partition)
import Text.XML.Light as X
import Control.Error
import Control.Monad (liftM)

loadXml :: String -> IO Element
loadXml filename = do
	contents <- readFile filename
	either fail return (parseXml contents)

parseXml :: String -> Either String Element
parseXml str = note "Invalid XML" (X.parseXMLDoc str)

getCommand :: Element -> Either String Command
getCommand e = do
	i <- getAttr "interface" e
	let c = hush $ getAttr "command" e
	return $ Command i c

getSelections :: Element -> Either String [Selection]
getSelections e = mapM getSelection' (X.findChildren selectionTag  e)
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

getSelection :: Element -> Either String Selection
getSelection e = do
	id <- getAttr "id" e
	iface <- getAttr "interface" e
	bindings <- return [] -- TODO
	commands <- return []
	let fromFeed = hush $ getAttr "from-feed" e
	(version, impl) <- getSelectionImpl e
	return Selection {
		selId = id
		,version = version
		,requires = []
		,fromFeed = fromFeed
		,selInterface = iface
		,selBindings = bindings
		,commands = commands
		,selImpl = impl
	}

getSelectionImpl :: Element -> Either String (AnyVersion, SelectionImpl)
getSelectionImpl e =
	case ((getAttr "package" e), (getAttr "local-path" e)) of
		(Right pkg, _) -> getPackageImpl pkg
		(_, Right localPath) -> getLocalImpl localPath
		_ -> getZiImpl
	where
		getPackageImpl p = do
			dists <- getAttr "distributions" e
			version <- versionText
			return (Left version, PackageSelectionImpl {
				package = p
				,distributions = dists
			})

		versionText = getAttr "version" e
		ziVersion = versionText >>= return . Right . parseVersion

		getLocalImpl localPath = do
			version <- ziVersion
			return (version, LocalSelectionImpl {
				localPath = localPath
			})

		getZiImpl = do
			digests <- getChild "manifest-digest" e >>= parseDigests
			version <- ziVersion
			return (version, SelectionImpl {
				digests = digests
			})

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
