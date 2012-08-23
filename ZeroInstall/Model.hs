module ZeroInstall.Model where

type Interface = String
type BindingName = String
type CommandName = Maybe String

data Command = Command Interface CommandName
	deriving Show

data Binding = Binding BindingName BindingValue
	deriving Show

data BindingValue =
	EnvironmentBinding BindingAlg EnvironmentValue
	| ExecutableInPath Command
	deriving Show

data BindingMode = Prepend | Append
	deriving Show

data BindingAlg = Replace | AddToExisting String BindingMode
	deriving Show

data EnvironmentValue = EnvValue String | EnvInsert String
	deriving Show

data Digest = Digest String String
	deriving Show
type Selections = [Selection]
type Selection = Implementation

data Implementation = Implementation {
	implId :: String,
	interface :: Interface,
	bindings :: [Binding],
	commands :: [Command],
	fromFeed :: Maybe Interface,
	implDetails :: ImplementationDetails,
	requires :: [Requirement]
} deriving Show

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

data Importance = Recommended
	deriving Show
data Requirement = Requirement Interface [Binding] (Maybe Importance)
	deriving Show
