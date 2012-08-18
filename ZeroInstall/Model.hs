module ZeroInstall.Model where

type Interface = String
type BindingName = String
type CommandName = Maybe String

data Command = Command Interface (Maybe String)
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
type Selections = [Selection]

data SelectionImpl = ZISelectionImpl {
		version :: Version,
		digests :: Digest,
		requires :: [Requirement]
	} | PackageSelectionImpl {
		package :: String,
		distributions :: String,
		from_feed :: String,
		versionString :: String
	}

data VersionPrefix = Pre | Post | RC
data VersionComponent = VersionComponent Int | PrefixedVersionComponent VersionPrefix (Maybe Int)
type Version = [VersionComponent]

data Selection = Selection {
		selId :: String,
		selInterface :: Interface,
		selBindings :: [Binding],
		commands :: [Command],
		selImpl :: SelectionImpl
}

data Importance = Recommended
data Requirement = Requirement {
	interface :: Interface,
	bindings :: [Binding],
	importance :: Maybe Importance
}
