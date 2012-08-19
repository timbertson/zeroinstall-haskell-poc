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

data SelectionImpl = ZISelectionImpl {
		version :: Version,
		digest :: Digest,
		requires :: [Requirement]
	} | PackageSelectionImpl {
		package :: String,
		distributions :: String,
		fromFeed :: String,
		versionString :: String
	}
	deriving Show

data VersionPrefix = Pre | Post | RC
	deriving Show
data VersionComponent = VersionComponent Int | PrefixedVersionComponent VersionPrefix (Maybe Int)
	deriving Show
type Version = [VersionComponent]

data Selection = Selection {
		selId :: String,
		selInterface :: Interface,
		selBindings :: [Binding],
		commands :: [Command],
		selImpl :: SelectionImpl
}
	deriving Show

data Importance = Recommended
	deriving Show
data Requirement = Requirement {
	interface :: Interface,
	bindings :: [Binding],
	importance :: Maybe Importance
}
	deriving Show
