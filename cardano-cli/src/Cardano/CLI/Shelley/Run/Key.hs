{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Key
  ( ShelleyKeyCmdError
  , SomeSigningKey(..)
  , renderShelleyKeyCmdError
  , runKeyCmd

    -- * Exports for testing
  , decodeBech32
  ) where

import           Control.Exception (Exception (..), IOException)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.Exit (exitFailure)

import qualified Control.Exception as Exception
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT)

import qualified Codec.Binary.Bech32 as Bech32

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Cardano.Ledger.Keys as Shelley

import           Cardano.Api
import qualified Cardano.Api.Byron as ByronApi
import           Cardano.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import           Cardano.Api.Shelley

import qualified Cardano.CLI.Byron.Key as Byron
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyTextOrFile (..),
                   VerificationKeyTextOrFileError, readVerificationKeyTextOrFileAnyOf,
                   renderVerificationKeyTextOrFileError)
import           Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..))


data ShelleyKeyCmdError
  = ShelleyKeyCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyKeyCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyKeyCmdWriteFileError !(FileError ())
  | ShelleyKeyCmdByronKeyFailure !Byron.ByronKeyFailure
  | ShelleyKeyCmdByronKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | ShelleyKeyCmdItnKeyConvError !ItnKeyConversionError
  | ShelleyKeyCmdWrongKeyTypeError
  | ShelleyKeyCmdCardanoAddressSigningKeyFileError
      !(FileError CardanoAddressSigningKeyConversionError)
  | ShelleyKeyCmdNonLegacyKey !FilePath
  | ShelleyKeyCmdExpectedExtendedVerificationKey SomeAddressVerificationKey
  | ShelleyKeyCmdVerificationKeyReadError VerificationKeyTextOrFileError
  deriving Show

renderShelleyKeyCmdError :: ShelleyKeyCmdError -> Text
renderShelleyKeyCmdError err =
  case err of
    ShelleyKeyCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdByronKeyFailure e -> Byron.renderByronKeyFailure e
    ShelleyKeyCmdByronKeyParseError errTxt -> errTxt
    ShelleyKeyCmdItnKeyConvError convErr -> renderConversionError convErr
    ShelleyKeyCmdWrongKeyTypeError -> Text.pack "Please use a signing key file \
                                   \when converting ITN BIP32 or Extended keys"
    ShelleyKeyCmdCardanoAddressSigningKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyKeyCmdNonLegacyKey fp -> "Signing key at: " <> Text.pack fp <> " is not a legacy Byron signing key and should \
                                    \ not need to be converted."
    ShelleyKeyCmdVerificationKeyReadError e -> renderVerificationKeyTextOrFileError e
    ShelleyKeyCmdExpectedExtendedVerificationKey someVerKey ->
      "Expected an extended verification key but got: " <> renderSomeAddressVerificationKey someVerKey

runKeyCmd :: KeyCmd -> ExceptT ShelleyKeyCmdError IO ()
runKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey skf vkf ->
      runGetVerificationKey skf vkf

    KeyNonExtendedKey evkf vkf ->
      runConvertToNonExtendedKey evkf vkf

    KeyConvertByronKey mPassword keytype skfOld skfNew ->
      runConvertByronKey mPassword keytype skfOld skfNew

    KeyConvertByronGenesisVKey oldVk newVkf ->
      runConvertByronGenesisVerificationKey oldVk newVkf

    KeyConvertITNStakeKey itnKeyFile outFile ->
      runConvertITNStakeKey itnKeyFile outFile
    KeyConvertITNExtendedToStakeKey itnPrivKeyFile outFile ->
      runConvertITNExtendedToStakeKey itnPrivKeyFile outFile
    KeyConvertITNBip32ToStakeKey itnPrivKeyFile outFile ->
      runConvertITNBip32ToStakeKey itnPrivKeyFile outFile

    KeyConvertCardanoAddressSigningKey keyType skfOld skfNew ->
      runConvertCardanoAddressSigningKey keyType skfOld skfNew

runGetVerificationKey :: SigningKeyFile
                      -> VerificationKeyFile
                      -> ExceptT ShelleyKeyCmdError IO ()
runGetVerificationKey skf (VerificationKeyFile vkf) = do
    ssk <- firstExceptT ShelleyKeyCmdReadKeyFileError $
             readSigningKeyFile skf
    withSomeSigningKey ssk $ \sk ->
      let vk = getVerificationKey sk in
      firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
        writeFileTextEnvelope vkf Nothing vk


data SomeSigningKey
  = AByronSigningKey           (SigningKey ByronKey)
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)
  | AVrfSigningKey             (SigningKey VrfKey)
  | AKesSigningKey             (SigningKey KesKey)

withSomeSigningKey :: SomeSigningKey
                   -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
                   -> a
withSomeSigningKey ssk f =
    case ssk of
      AByronSigningKey           sk -> f sk
      APaymentSigningKey         sk -> f sk
      APaymentExtendedSigningKey sk -> f sk
      AStakeSigningKey           sk -> f sk
      AStakeExtendedSigningKey   sk -> f sk
      AStakePoolSigningKey       sk -> f sk
      AGenesisSigningKey         sk -> f sk
      AGenesisExtendedSigningKey sk -> f sk
      AGenesisDelegateSigningKey sk -> f sk
      AGenesisDelegateExtendedSigningKey
                                 sk -> f sk
      AGenesisUTxOSigningKey     sk -> f sk
      AVrfSigningKey             sk -> f sk
      AKesSigningKey             sk -> f sk

readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (FileError InputDecodeError) IO SomeSigningKey
readSigningKeyFile (SigningKeyFile skFile) =
    newExceptT $
      readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                      AByronSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                      AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                      AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                      AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                      AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                      AGenesisUTxOSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]


runConvertToNonExtendedKey
  :: VerificationKeyFile
  -> VerificationKeyFile
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertToNonExtendedKey evkf (VerificationKeyFile vkf) =
  writeVerificationKey =<< readExtendedVerificationKeyFile evkf
 where
  -- TODO: Expose a function specifically for this purpose
  -- and explain the extended verification keys can be converted
  -- to their non-extended counterparts however this is NOT the case
  -- for extended signing keys

  writeVerificationKey
    :: SomeAddressVerificationKey
    -> ExceptT ShelleyKeyCmdError IO ()
  writeVerificationKey ssk =
    case ssk of
      APaymentExtendedVerificationKey vk ->
        writeToDisk vkf (castVerificationKey vk :: VerificationKey PaymentKey)
      AStakeExtendedVerificationKey vk ->
        writeToDisk vkf (castVerificationKey vk :: VerificationKey StakeKey)
      AGenesisExtendedVerificationKey vk ->
        writeToDisk vkf (castVerificationKey vk :: VerificationKey GenesisKey)
      AGenesisDelegateExtendedVerificationKey vk ->
        writeToDisk vkf (castVerificationKey vk :: VerificationKey GenesisDelegateKey)
      nonExtendedKey -> left $ ShelleyKeyCmdExpectedExtendedVerificationKey nonExtendedKey


  writeToDisk
   :: Key keyrole
   => FilePath -> VerificationKey keyrole -> ExceptT ShelleyKeyCmdError IO ()
  writeToDisk vkf' vk = firstExceptT ShelleyKeyCmdWriteFileError . newExceptT
                          $ writeFileTextEnvelope vkf' Nothing vk


readExtendedVerificationKeyFile
  :: VerificationKeyFile
  -> ExceptT ShelleyKeyCmdError IO SomeAddressVerificationKey
readExtendedVerificationKeyFile evkfile = do
  vKey <- firstExceptT ShelleyKeyCmdVerificationKeyReadError
            . newExceptT $ readVerificationKeyTextOrFileAnyOf
                         $ VktofVerificationKeyFile evkfile
  case vKey of
      k@APaymentExtendedVerificationKey{} -> return k
      k@AStakeExtendedVerificationKey{} -> return k
      k@AGenesisExtendedVerificationKey{} -> return k
      k@AGenesisDelegateExtendedVerificationKey{} -> return k
      nonExtendedKey ->
        left $ ShelleyKeyCmdExpectedExtendedVerificationKey nonExtendedKey


runConvertByronKey
  :: Maybe Text      -- ^ Password (if applicable)
  -> ByronKeyType
  -> SomeKeyFile     -- ^ Input file: old format
  -> OutputFile      -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertByronKey mPwd (ByronPaymentKey format) (ASigningKeyFile skeyPathOld) =
    convertByronSigningKey mPwd format convert skeyPathOld
  where
    convert :: Byron.SigningKey -> SigningKey ByronKey
    convert = ByronSigningKey

runConvertByronKey mPwd (ByronGenesisKey format) (ASigningKeyFile skeyPathOld) =
    convertByronSigningKey mPwd format convert skeyPathOld
  where
    convert :: Byron.SigningKey -> SigningKey GenesisExtendedKey
    convert (Byron.SigningKey xsk) = GenesisExtendedSigningKey xsk

runConvertByronKey mPwd (ByronDelegateKey format) (ASigningKeyFile skeyPathOld) =
    convertByronSigningKey mPwd format convert skeyPathOld
  where
    convert :: Byron.SigningKey -> SigningKey GenesisDelegateExtendedKey
    convert (Byron.SigningKey xsk) = GenesisDelegateExtendedSigningKey xsk

runConvertByronKey _ (ByronPaymentKey NonLegacyByronKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertByronVerificationKey convert vkeyPathOld
  where
    convert :: Byron.VerificationKey -> VerificationKey ByronKey
    convert = ByronVerificationKey

runConvertByronKey _ (ByronGenesisKey NonLegacyByronKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertByronVerificationKey convert vkeyPathOld
  where
    convert :: Byron.VerificationKey -> VerificationKey GenesisExtendedKey
    convert (Byron.VerificationKey xvk) = GenesisExtendedVerificationKey xvk

runConvertByronKey _ (ByronDelegateKey NonLegacyByronKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertByronVerificationKey convert vkeyPathOld
  where
    convert :: Byron.VerificationKey
            -> VerificationKey GenesisDelegateExtendedKey
    convert (Byron.VerificationKey xvk) =
      GenesisDelegateExtendedVerificationKey xvk

runConvertByronKey _ (ByronPaymentKey  LegacyByronKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertByronKey _ (ByronGenesisKey  LegacyByronKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertByronKey _ (ByronDelegateKey LegacyByronKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

legacyVerificationKeysNotSupported :: ExceptT e IO a
legacyVerificationKeysNotSupported =
    liftIO $ do
      putStrLn $ "convert keys: byron legacy format not supported for "
              ++ "verification keys. Convert the signing key and then get the "
              ++ "verification key."
      exitFailure


convertByronSigningKey
  :: forall keyrole.
     Key keyrole
  => Maybe Text          -- ^ Password (if applicable)
  -> ByronKeyFormat
  -> (Byron.SigningKey -> SigningKey keyrole)
  -> SigningKeyFile      -- ^ Input file: old format
  -> OutputFile          -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
convertByronSigningKey mPwd byronFormat convert
                       skeyPathOld
                       (OutputFile skeyPathNew) = do


    sKey <- firstExceptT ShelleyKeyCmdByronKeyFailure
              $ Byron.readByronSigningKey byronFormat skeyPathOld

    -- Account for password protected legacy Byron keys
    unprotectedSk <- case sKey of
                       ByronApi.AByronSigningKeyLegacy (ByronSigningKeyLegacy sk@(Crypto.SigningKey xprv)) ->
                         case mPwd of
                           -- Change password to empty string
                           Just pwd -> return . Crypto.SigningKey
                                         $ Crypto.xPrvChangePass (Text.encodeUtf8 pwd) (Text.encodeUtf8 "") xprv
                           Nothing -> return sk
                       ByronApi.AByronSigningKey (ByronSigningKey sk) -> return sk


    let sk' :: SigningKey keyrole
        sk' = convert unprotectedSk

    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope skeyPathNew Nothing sk'

convertByronVerificationKey
  :: forall keyrole.
     Key keyrole
  => (Byron.VerificationKey -> VerificationKey keyrole)
  -> VerificationKeyFile -- ^ Input file: old format
  -> OutputFile          -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
convertByronVerificationKey convert
                            (VerificationKeyFile vkeyPathOld)
                            (OutputFile vkeyPathNew) = do

    vk <- firstExceptT ShelleyKeyCmdByronKeyFailure $
            Byron.readPaymentVerificationKey (Byron.VerificationKeyFile vkeyPathOld)

    let vk' :: VerificationKey keyrole
        vk' = convert vk

    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'


runConvertByronGenesisVerificationKey
  :: VerificationKeyBase64  -- ^ Input key raw old format
  -> OutputFile             -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertByronGenesisVerificationKey (VerificationKeyBase64 b64ByronVKey)
                                      (OutputFile vkeyPathNew) = do

    vk <- firstExceptT (ShelleyKeyCmdByronKeyParseError . textShow)
        . hoistEither
        . Byron.Crypto.parseFullVerificationKey
        . Text.pack
        $ b64ByronVKey

    let vk' :: VerificationKey GenesisKey
        vk' = convert vk

    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'
  where
    convert :: Byron.VerificationKey -> VerificationKey GenesisKey
    convert (Byron.VerificationKey xvk) =
      castVerificationKey (GenesisExtendedVerificationKey xvk)


--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runConvertITNStakeKey
  :: SomeKeyFile
  -> OutputFile
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertITNStakeKey (AVerificationKeyFile (VerificationKeyFile vk)) (OutputFile outFile) = do
  bech32publicKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $
                     readFileITNKey vk
  vkey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNVerificationKey bech32publicKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope outFile Nothing vkey

runConvertITNStakeKey (ASigningKeyFile (SigningKeyFile sk)) (OutputFile outFile) = do
  bech32privateKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $
                      readFileITNKey sk
  skey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNSigningKey bech32privateKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope outFile Nothing skey

runConvertITNExtendedToStakeKey :: SomeKeyFile -> OutputFile -> ExceptT ShelleyKeyCmdError IO ()
runConvertITNExtendedToStakeKey (AVerificationKeyFile _) _ = left ShelleyKeyCmdWrongKeyTypeError
runConvertITNExtendedToStakeKey (ASigningKeyFile (SigningKeyFile sk)) (OutputFile outFile) = do
  bech32privateKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $ readFileITNKey sk
  skey <- hoistEither . first ShelleyKeyCmdItnKeyConvError
            $ convertITNExtendedSigningKey bech32privateKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT
    $ writeFileTextEnvelope outFile Nothing skey

runConvertITNBip32ToStakeKey :: SomeKeyFile -> OutputFile -> ExceptT ShelleyKeyCmdError IO ()
runConvertITNBip32ToStakeKey (AVerificationKeyFile _) _ = left ShelleyKeyCmdWrongKeyTypeError
runConvertITNBip32ToStakeKey (ASigningKeyFile (SigningKeyFile sk)) (OutputFile outFile) = do
  bech32privateKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $ readFileITNKey sk
  skey <- hoistEither . first ShelleyKeyCmdItnKeyConvError
            $ convertITNBIP32SigningKey bech32privateKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT
    $ writeFileTextEnvelope outFile Nothing skey

-- | An error that can occur while converting an Incentivized Testnet (ITN)
-- key.
data ItnKeyConversionError
  = ItnKeyBech32DecodeError !Bech32DecodeError
  | ItnReadBech32FileError !FilePath !IOException
  | ItnSigningKeyDeserialisationError !ByteString
  | ItnVerificationKeyDeserialisationError !ByteString
  deriving Show

-- | Render an error message for an 'ItnKeyConversionError'.
renderConversionError :: ItnKeyConversionError -> Text
renderConversionError err =
  case err of
    ItnKeyBech32DecodeError decErr ->
      "Error decoding Bech32 key: " <> Text.pack (displayError decErr)
    ItnReadBech32FileError fp readErr ->
      "Error reading Bech32 key at: " <> textShow fp
                        <> " Error: " <> Text.pack (displayException readErr)
    ItnSigningKeyDeserialisationError _sKey ->
      -- Sensitive data, such as the signing key, is purposely not included in
      -- the error message.
      "Error deserialising signing key."
    ItnVerificationKeyDeserialisationError vKey ->
      "Error deserialising verification key: " <> textShow (BSC.unpack vKey)

-- | Convert public ed25519 key to a Shelley stake verification key
convertITNVerificationKey :: Text -> Either ItnKeyConversionError (VerificationKey StakeKey)
convertITNVerificationKey pubKey = do
  (_, _, keyBS) <- first ItnKeyBech32DecodeError (decodeBech32 pubKey)
  case DSIGN.rawDeserialiseVerKeyDSIGN keyBS of
    Just verKey -> Right . StakeVerificationKey $ Shelley.VKey verKey
    Nothing -> Left $ ItnVerificationKeyDeserialisationError keyBS

-- | Convert private ed22519 key to a Shelley signing key.
convertITNSigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeKey)
convertITNSigningKey privKey = do
  (_, _, keyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  case DSIGN.rawDeserialiseSignKeyDSIGN keyBS of
    Just signKey -> Right $ StakeSigningKey signKey
    Nothing -> Left $ ItnSigningKeyDeserialisationError keyBS

-- | Convert extended private ed22519 key to a Shelley signing key
-- Extended private key = 64 bytes,
-- Public key = 32 bytes.
convertITNExtendedSigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeExtendedKey)
convertITNExtendedSigningKey privKey = do
  (_, _, privkeyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  let dummyChainCode = BS.replicate 32 0
  case xPrvFromBytes $ BS.concat [privkeyBS, dummyChainCode] of
    Just xprv -> Right $ StakeExtendedSigningKey xprv
    Nothing -> Left $ ItnSigningKeyDeserialisationError privkeyBS

-- BIP32 Private key = 96 bytes (64 bytes extended private key + 32 bytes chaincode)
-- BIP32 Public Key = 64 Bytes
convertITNBIP32SigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeExtendedKey)
convertITNBIP32SigningKey privKey = do
  (_, _, privkeyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  case xPrvFromBytes privkeyBS of
    Just xprv -> Right $ StakeExtendedSigningKey xprv
    Nothing -> Left $ ItnSigningKeyDeserialisationError privkeyBS

readFileITNKey :: FilePath -> IO (Either ItnKeyConversionError Text)
readFileITNKey fp = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> return . Left $ ItnReadBech32FileError fp e
    Right str -> return . Right . Text.concat $ Text.words $ Text.pack str

--------------------------------------------------------------------------------
-- `cardano-address` extended signing key conversions
--------------------------------------------------------------------------------

runConvertCardanoAddressSigningKey
  :: CardanoAddressKeyType
  -> SigningKeyFile
  -> OutputFile
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertCardanoAddressSigningKey keyType skFile (OutputFile outFile) = do
  sKey <- firstExceptT ShelleyKeyCmdCardanoAddressSigningKeyFileError
    . newExceptT
    $ readSomeCardanoAddressSigningKeyFile keyType skFile
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT
    $ writeSomeCardanoAddressSigningKeyFile outFile sKey

-- | Some kind of signing key that was converted from a @cardano-address@
-- signing key.
data SomeCardanoAddressSigningKey
  = ACardanoAddrShelleyPaymentSigningKey !(SigningKey PaymentExtendedKey)
  | ACardanoAddrShelleyStakeSigningKey !(SigningKey StakeExtendedKey)
  | ACardanoAddrByronSigningKey !(SigningKey ByronKey)

-- | An error that can occur while converting a @cardano-address@ extended
-- signing key.
data CardanoAddressSigningKeyConversionError
  = CardanoAddressSigningKeyBech32DecodeError !Bech32DecodeError
  -- ^ There was an error in decoding the string as Bech32.
  | CardanoAddressSigningKeyDeserialisationError !ByteString
  -- ^ There was an error in converting the @cardano-address@ extended signing
  -- key.
  deriving (Show, Eq)

instance Error CardanoAddressSigningKeyConversionError where
  displayError = Text.unpack . renderCardanoAddressSigningKeyConversionError

-- | Render an error message for a 'CardanoAddressSigningKeyConversionError'.
renderCardanoAddressSigningKeyConversionError
  :: CardanoAddressSigningKeyConversionError
  -> Text
renderCardanoAddressSigningKeyConversionError err =
  case err of
    CardanoAddressSigningKeyBech32DecodeError decErr ->
      Text.pack (displayError decErr)
    CardanoAddressSigningKeyDeserialisationError _bs ->
      -- Sensitive data, such as the signing key, is purposely not included in
      -- the error message.
      "Error deserialising cardano-address signing key."

-- | Decode a Bech32-encoded string.
decodeBech32
  :: Text
  -> Either Bech32DecodeError (Bech32.HumanReadablePart, Bech32.DataPart, ByteString)
decodeBech32 bech32Str =
  case Bech32.decodeLenient bech32Str of
    Left err -> Left (Bech32DecodingError err)
    Right (hrPart, dataPart) ->
      case Bech32.dataPartToBytes dataPart of
        Nothing ->
          Left $ Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)
        Just bs -> Right (hrPart, dataPart, bs)

-- | Convert a Ed25519 BIP32 extended signing key (96 bytes) to a @cardano-crypto@
-- style extended signing key.
--
-- Note that both the ITN and @cardano-address@ use this key format.
convertBip32SigningKey
  :: ByteString
  -> Either CardanoAddressSigningKeyConversionError Crypto.XPrv
convertBip32SigningKey signingKeyBs =
  case xPrvFromBytes signingKeyBs of
    Just xPrv -> Right xPrv
    Nothing ->
      Left $ CardanoAddressSigningKeyDeserialisationError signingKeyBs

-- | Read a file containing a Bech32-encoded Ed25519 BIP32 extended signing
-- key.
readBech32Bip32SigningKeyFile
  :: SigningKeyFile
  -> IO (Either (FileError CardanoAddressSigningKeyConversionError) Crypto.XPrv)
readBech32Bip32SigningKeyFile (SigningKeyFile fp) = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> pure . Left $ FileIOError fp e
    Right str ->
      case decodeBech32 (Text.concat $ Text.words $ Text.pack str) of
        Left err ->
          pure $ Left $
            FileError fp (CardanoAddressSigningKeyBech32DecodeError err)
        Right (_hrPart, _dataPart, bs) ->
          pure $ first (FileError fp) (convertBip32SigningKey bs)

-- | Read a file containing a Bech32-encoded @cardano-address@ extended
-- signing key.
readSomeCardanoAddressSigningKeyFile
  :: CardanoAddressKeyType
  -> SigningKeyFile
  -> IO (Either (FileError CardanoAddressSigningKeyConversionError) SomeCardanoAddressSigningKey)
readSomeCardanoAddressSigningKeyFile keyType skFile = do
    xPrv <- readBech32Bip32SigningKeyFile skFile
    pure (toSomeCardanoAddressSigningKey <$> xPrv)
  where
    toSomeCardanoAddressSigningKey :: Crypto.XPrv -> SomeCardanoAddressSigningKey
    toSomeCardanoAddressSigningKey xPrv =
      case keyType of
        CardanoAddressShelleyPaymentKey ->
          ACardanoAddrShelleyPaymentSigningKey
            (PaymentExtendedSigningKey xPrv)
        CardanoAddressShelleyStakeKey ->
          ACardanoAddrShelleyStakeSigningKey (StakeExtendedSigningKey xPrv)
        CardanoAddressIcarusPaymentKey ->
          ACardanoAddrByronSigningKey $
            ByronSigningKey (Byron.SigningKey xPrv)
        CardanoAddressByronPaymentKey ->
          ACardanoAddrByronSigningKey $
            ByronSigningKey (Byron.SigningKey xPrv)

-- | Write a text envelope formatted file containing a @cardano-address@
-- extended signing key, but converted to a format supported by @cardano-cli@.
writeSomeCardanoAddressSigningKeyFile
  :: FilePath
  -> SomeCardanoAddressSigningKey
  -> IO (Either (FileError ()) ())
writeSomeCardanoAddressSigningKeyFile outFile skey =
  case skey of
    ACardanoAddrShelleyPaymentSigningKey sk ->
      writeFileTextEnvelope outFile Nothing sk
    ACardanoAddrShelleyStakeSigningKey sk ->
      writeFileTextEnvelope outFile Nothing sk
    ACardanoAddrByronSigningKey sk ->
      writeFileTextEnvelope outFile Nothing sk
