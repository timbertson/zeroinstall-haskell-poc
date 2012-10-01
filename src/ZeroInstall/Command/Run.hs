module ZeroInstall.Command.Run where
import Options.Applicative
import ZeroInstall.App (lookupApp, runApp, Config(..))

-- for arguments_
import Control.Monad
import Data.Maybe
import Data.Monoid
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Types
import Debug.Trace

skipOpts :: (String -> Maybe a) -> String -> Maybe a
skipOpts _ ('-':_) = Nothing
skipOpts rdr s = rdr s



traceM_ :: Monad m => String -> m ()
traceM_ = flip trace $ return ()

traceM :: Monad m => (a -> String) -> a -> m a
traceM msg v = trace (msg v) (return v)

data Args = RunArgs {
	console :: Bool,
	dry_run :: Bool,
	gui :: Bool,
	verbose :: Bool,
	with_store :: [String],
	before :: Maybe String,
	command :: Maybe String,
	cpu :: Maybe String,
	message :: Maybe String,
	not_before :: Maybe String,
	offline :: Bool,
	os :: Maybe String,
	refresh :: Bool,
	source :: Bool,
	main_ :: Maybe String,
	wrapper :: Maybe String,
	-- runnable :: String,
	args_ :: [String]
} deriving (Show, Eq)

optionalStr = optional . strOption
strOptions = many . strOption

runArgs :: Parser Args
runArgs = RunArgs
	<$> switch         ( long "console"    & help "never use GUI"                                   & short 'c' )
	<*> switch         ( long "dry-run"    & help "just print what would be executed" )
	<*> switch         ( long "gui"        & help "show graphical policy editor"                    & short 'g' )
	<*> switch         ( long "verbose"    & help "more verbose output"                             & short 'v' )
	<*> strOptions     ( long "with_store" & help "add an implementation cache"                     & metavar "DIR" )
	<*> optionalStr    ( long "before"     & help "choose a version before this" )
	<*> optionalStr    ( long "command"    & help "command to select" )
	<*> optionalStr    ( long "cpu"        & help "target CPU type" )
	<*> optionalStr    ( long "message"    & help "message to display when interacting with user" )
	<*> optionalStr    ( long "not_before" & help "minimum version to choose" )
	<*> switch         ( long "offline"    & help "try to avoid using the network"                  & short 'o' )
	<*> optionalStr    ( long "os"         & help "target operation system type" )
	<*> switch         ( long "refresh"    & help "refresh all used interfaces"                     & short 'r' )
	<*> switch         ( long "source"     & help "select source code"                              & short 's' )
	<*> optionalStr    ( long "main"       & help "name of the file to execute"                     & short 'm' )
	<*> optionalStr    ( long "wrapper"    & help "execute program using a debugger, etc"           & short 'w' )
	-- <*> argument Just  ( metavar "URI"     & help "runnable" )
	<*> argumentsStrict Just ( metavar "ARGS"    & help "args" )

-- | Builder for an argument list parser. All arguments are collected and returned as a list.
--
-- All arguments (even those starting with "-") appearing after the first plain argument are included in the result
--
-- (where a "plain" argument is one equal to "--", or not starting with "-")
--
-- This parser accepts a special argument: @--@. When a @--@ is found on the
-- command line, all following arguments are included in the result, even if
-- they start with @'-'@.
argumentsStrict :: Show a => (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
argumentsStrict = arguments_ True
arguments1Strict :: Show a => (String -> Maybe a) -> Mod ArgumentFields [a] -> Parser [a]
arguments1Strict = arguments_ False
-- arguments1Strict = arguments_ True
-- see https://github.com/pcapriotti/optparse-applicative/issues/22
arguments_ allow_empty p m = set_default <$> fromM args1
	where
		Mod f (DefaultProp def sdef) g = m
		show_def = sdef <*> def

		props = mkProps mempty g
		props' = (mkProps mempty g) { propShowDefault = show_def }

		args1 | allow_empty = args
		      | otherwise = do
			mx <- oneM arg_or_ddash
			case mx of
				Nothing -> someM arg
				Just x  -> (x:) <$> manyM arg

		args = do
			mx <- oneM $ optional arg_or_ddash
			case mx of
				Nothing       -> return []
				Just Nothing  -> manyM arg
				Just (Just x) -> (x:) <$> manyM arg

		arg_or_ddash = (ddash *> pure Nothing) <|> (p <$> nonOpt)
		set_default [] = fromMaybe [] def
		set_default xs = xs

		arg = liftOpt (Option (ArgReader (CReader compl p)) props)

		ddash :: Parser ()
		ddash = argument' (guard . (== "--")) internal

		nonOpt = argument' nonOpt' internal
		nonOpt' ('-':_) = Nothing
		nonOpt' a = Just a

		ArgumentFields compl = f (ArgumentFields mempty)

main = do
	parsed <- execParser opts
	let (runnable:args) = args_ parsed
	print parsed
	app <- lookupApp (Config {}) runnable
	case app of
		(Left msg) -> print (msg)
		(Right app) -> runApp app args
	where
		opts = info (helper <*> runArgs)
			( fullDesc)
