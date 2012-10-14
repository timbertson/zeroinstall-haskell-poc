{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.Lazy as Base64L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSUTF8

import qualified Data.ByteString.Lazy as LZ
import qualified Data.Binary as B
import qualified Data.Binary.Get as B

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromJust)
import Control.Monad (unless)
import ZeroInstall.Utils

extractBase64 :: Text.Text -> Either String LZ.ByteString
extractBase64 content = prependErrorMessage "Unable to decode Base64 data: " $ Base64L.decode (TL.encodeUtf8 base64_text)
	where
		(base64_parts, checksum_parts) = break (isChecksum) $ map (Text.strip) (Text.lines content)
		isChecksum t = (Text.take 1 t) == "="
		--TODO: verify checksum
		base64_text = TL.fromChunks $ base64_parts
	
textWithin :: Text.Text -> Text.Text -> Text.Text -> Text.Text
textWithin start end doc = content where
		[_, after_header] = Text.splitOn start doc
		[content, _] = Text.splitOn end after_header


emptyLine l = (Text.strip l) == Text.empty

extractPublicKey :: String -> Text.Text -> Either String OpenPGP.Packet
extractPublicKey keyId fileContents = do
	key_bin <- extractBase64 (Text.unlines keyBase64Contents)
	let keyPkt = B.decode key_bin
	let shortKeyFingerprint = (reverse (take 16 (reverse (OpenPGP.fingerprint keyPkt))))
	if (shortKeyFingerprint == keyId)
		then Right keyPkt
		else (Left $ "Key ID mismatch: " ++ shortKeyFingerprint ++ " != " ++ keyId)
	where
		(pgpStart, pgpEnd) = ("-----BEGIN PGP PUBLIC KEY BLOCK-----", "-----END PGP PUBLIC KEY BLOCK-----")
		keyBlock = Text.lines (textWithin pgpStart pgpEnd fileContents)
		keyBase64Contents = (dropWhile (not . emptyLine)) (dropWhile emptyLine keyBlock)

main = do
	entireContents <- Text.readFile "feed.xml"
	let sigSep = "<!-- Base64 Signature"
	let (content, sig_base64_comment) = Text.breakOn sigSep entireContents
	sig_bin <- requireRight $ extractBase64 (textWithin sigSep "-->" sig_base64_comment)
	let sigPkt = B.decode sig_bin
	-- print ("sigpkt", sigPkt)
	-- let keyId = drop 20 (OpenPGP.fingerprint sigPkt)
	let keyId = fromJust $ OpenPGP.signature_issuer sigPkt
	print keyId
	let dataPkt = OpenPGP.LiteralDataPacket {
			OpenPGP.format = 'u',
			OpenPGP.filename = "(xml)",
			OpenPGP.timestamp = 1234,
			OpenPGP.content = TL.encodeUtf8 (TL.fromStrict content)
		}
	keyContents <- Text.readFile (keyId ++ ".gpg")
	keyPkt <- requireRight $ extractPublicKey keyId keyContents
	-- remove the GPG header from within base64 block
	let sigmsg = (OpenPGP.Message [sigPkt, dataPkt])
	-- TODO...
	putStrLn " -- Here goes!"
	print $ ("verify", OpenPGP.verify (OpenPGP.Message [keyPkt]) sigmsg 0)
	return ()

