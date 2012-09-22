module ZeroInstall.Model where

import qualified Data.Text as T
import Data.List (intercalate)

type Interface = String
type BindingName = String
type CommandName = String
type LocalPath = [String]
type Separator = String

splitPath = (map T.unpack) . (T.split (== '/')) . T.pack

data RunSpecification = RunSpecification Interface (Maybe CommandName)

data Command = Command {
	commandBindings :: [Binding],
	commandRequirements :: [Requirement],
	commandName :: CommandName,
	commandPath :: Maybe FilePath,
	commandArgs :: [String],
	runner :: Maybe Runner
	} deriving Show

data Runner = Runner {
	runnerInterface :: Interface,
	runnerArgs :: [String],
	runnerCommand :: Maybe CommandName
	} deriving Show

defaultCommand = maybe "run" id

class HasBindings a where
	bindings :: a -> [Binding]

class HasRequirements a where
	requirements :: a -> [Requirement]

instance HasBindings Command where
	bindings = commandBindings

instance HasRequirements Command where
	requirements = commandRequirements

data Binding = Binding BindingName BindingValue
	deriving Show

data BindingValue =
	EnvironmentBinding BindingAlg EnvironmentValue (Maybe Separator)
	| ExecutableInPath Command
	deriving Show

data BindingMode = Prepend | Append
	deriving Show

data BindingAlg = Replace | AddToExisting BindingMode
	deriving Show

lookupBindingAlg :: String -> Either String BindingAlg
lookupBindingAlg "prepend" = Right $ AddToExisting Prepend
lookupBindingAlg "append"  = Right $ AddToExisting Append
lookupBindingAlg "replace" = Right $ Replace
lookupBindingAlg unknown   = Left  $ "No such binding mode: " ++ unknown

defaultBindingAlg = AddToExisting Append

data EnvironmentValue = EnvValue String | EnvInsert String
	deriving Show

data Digest = Digest String String
	deriving Show

parseDigest :: String -> Either String Digest
parseDigest str =
	if (any null [alg, val])
		then Left $ "Invalid digest format: " ++ str
		else Right $ Digest alg val
	where
		(alg, val) = (break (`elem` ['=','_']) str)

formatDigest :: Digest -> String
formatDigest (Digest alg val) = intercalate sep [alg, val]
	where
		sep | alg `elem` ["sha1", "sha1new", "sha256"] = "="
		    | otherwise                                = "_"

data Selections = Selections Interface CommandName [Selection]

data LocatedSelection = LocatedSelection {
	selection :: Selection,
	location :: (Maybe FilePath)
}

instance HasInterface LocatedSelection where interface = interface . selection

type Selection = Implementation
getCommand :: Selections -> CommandName
getCommand (Selections _ c _) = c

data Implementation = Implementation {
	implId :: String,
	implInterface :: Interface,
	implBindings :: [Binding],
	commands :: [Command],
	fromFeed :: Maybe Interface,
	implDetails :: ImplementationDetails,
	implRequirements :: [Requirement]
} deriving Show

instance HasBindings Implementation where
	bindings = implBindings

instance HasRequirements Implementation where
	requirements = implRequirements

class HasInterface a where
	interface :: a -> Interface

instance HasInterface Implementation where
	interface = implInterface

data ImplementationDetails = Local LocalImplementation | Package PackageImplementation | Remote RemoteImplementation
	deriving Show

data RemoteImplementation = RemoteImplementation Version [Digest]
	deriving Show

data LocalImplementation = LocalImplementation Version FilePath
	deriving Show

data PackageImplementation = PackageImplementation {
		packageName :: String,
		packageVersion :: String,
		distributions :: [String]
	}
	deriving Show

instance HasVersionString RemoteImplementation where versionString (RemoteImplementation v _) = show v
instance HasVersionString LocalImplementation where versionString (LocalImplementation v _) = show v
instance HasVersionString PackageImplementation where versionString = packageVersion

instance HasVersion RemoteImplementation where version (RemoteImplementation v _) = v
instance HasVersion LocalImplementation where version (LocalImplementation v _) = v

class HasVersionString impl where
	versionString :: impl -> String

class HasVersion impl where
	version :: impl -> Version

data VersionPrefix = Pre | Post | RC
	deriving Show
data VersionComponent = VersionComponent Int | PrefixedVersionComponent VersionPrefix (Maybe Int)
	deriving Show
type Version = [VersionComponent]
type AnyVersion = Either String Version

data Importance = Required | Recommended
	deriving Show
data Requirement = Requirement Interface [Binding] (Maybe Importance)
	deriving Show

instance HasBindings Requirement where
	bindings (Requirement _ b _) = b

instance HasInterface Requirement where
	interface (Requirement iface _ _) = iface

parseImportance :: String -> Either String Importance
parseImportance "required"    = Right Required
parseImportance "recommended" = Right Recommended
parseImportance x             = Left $ "Unknown importance: " ++ x

