module ZeroInstall.Model where

import qualified Data.Text as T

type Interface = String
type BindingName = String
type CommandName = Maybe String
type LocalPath = [String]
type Separator = String

splitPath = (map T.unpack) . (T.split (== '/')) . T.pack

data RunSpecification = RunSpecification Interface CommandName

data Command = Command {
	commandBindings :: [Binding],
	commandRequirements :: [Requirement],
	commandName :: CommandName
	} deriving Show

class ImplementationExports a where
	bindings :: a -> [Binding]
	requirements :: a -> [Requirement]

instance ImplementationExports Command where
	bindings = commandBindings
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
data Selections = Selections Interface CommandName [Selection]
type Selection = Implementation
getCommand :: Selections -> CommandName
getCommand (Selections _ c _) = c

data Implementation = Implementation {
	implId :: String,
	interface :: Interface,
	implBindings :: [Binding],
	commands :: [Command],
	fromFeed :: Maybe Interface,
	implDetails :: ImplementationDetails,
	implRequirements :: [Requirement]
} deriving Show

instance ImplementationExports Implementation where
	bindings = implBindings
	requirements = implRequirements

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
