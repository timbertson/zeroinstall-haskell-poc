{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Selections where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle
import ZeroInstall.Model

data Test = Test {
	tinterface:: String,
	tversion:: Int
} deriving Show

instance XmlPickler Test where
	xpickle = xpTest

xpTest :: PU Test
xpTest = xpElem "TEST"
	(xpWrap
		(umt , extractFields)
		(xpPair (xpAttr "interface" xpText) (xpAttr "version" xpInt))
	)

xmlstr = "<TEST interface='python' version='2' extra='fail?'/>"
opts = []
-- runX (readString opts xmlstr >>> this)

-- 	where
umt :: (String, Int) -> Test
umt = uncurry mkTest
mkTest :: String -> Int -> Test
mkTest i v = Test { tinterface = i, tversion=v}
extractFields :: Test -> (String, Int)
extractFields t = (tinterface t, tversion t)



-- load :: FilePath -> Selections
-- load = fromXML . gg
-- fromXML :: String -> Selections

-- processSelection :: XmlNode -> Selection
-- 
-- processZiSelection :: XmlNode -> Selection
-- processPackageSelection :: XmlNode -> Selection
-- 
-- processBinding :: XmlNode -> Binding
-- processDigest :: XmlNode -> [Digest]
-- processCommand :: XmlNode -> Command
-- processRequires :: XmlNode -> Command
