{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.DeserialiseAnyOf
  ( InputFormat (..)
  , InputDecodeError (..)
  , deserialiseInput
  , deserialiseInputAnyOf
  , renderInputDecodeError

  -- TODO: Consider moving everything below
  , SomeAddressVerificationKey(..)
  , deserialiseAnyVerificationKey
  , deserialiseAnyVerificationKeyBech32
  , deserialiseAnyVerificationKeyTextEnvelope
  ) where

import           Prelude

import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toLower)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api.Error
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope

-- TODO: Think about if these belong
import           Cardano.Api.Address
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley

------------------------------------------------------------------------------
-- Formatted/encoded input deserialisation
------------------------------------------------------------------------------

-- | Input format/encoding.
data InputFormat a where
  -- | Bech32 encoding.
  InputFormatBech32 :: SerialiseAsBech32 a => InputFormat a

  -- | Hex/Base16 encoding.
  InputFormatHex :: SerialiseAsRawBytes a => InputFormat a

  -- TODO: Specify TextEnvelope CBOR hex
  -- | Text envelope format.
  InputFormatTextEnvelope :: HasTextEnvelope a => InputFormat a

  -- TODO: Add constructor for TextEnvelope Bech32

-- | Input decoding error.
data InputDecodeError
  = InputTextEnvelopeError !TextEnvelopeError
  -- ^ The provided data seems to be a valid text envelope, but some error
  -- occurred in deserialising it.
  | InputBech32DecodeError !Bech32DecodeError
  -- ^ The provided data is valid Bech32, but some error occurred in
  -- deserialising it.
  | InputInvalidError
  -- ^ The provided data does not represent a valid value of the provided
  -- type.
  deriving (Eq, Show)

instance Error InputDecodeError where
  displayError = Text.unpack . renderInputDecodeError

-- | Render an error message for a 'InputDecodeError'.
renderInputDecodeError :: InputDecodeError -> Text
renderInputDecodeError err =
  case err of
    InputTextEnvelopeError textEnvErr ->
      Text.pack (displayError textEnvErr)
    InputBech32DecodeError decodeErr ->
      Text.pack (displayError decodeErr)
    InputInvalidError -> "Invalid key."

-- | The result of a deserialisation function.
--
-- Note that this type isn't intended to be exported, but only used as a
-- helper within the 'deserialiseInput' function.
data DeserialiseInputResult a
  = DeserialiseInputSuccess !a
  -- ^ Input successfully deserialised.
  | DeserialiseInputError !InputDecodeError
  -- ^ The provided data is of the expected format/encoding, but an error
  -- occurred in deserialising it.
  | DeserialiseInputErrorFormatMismatch
  -- ^ The provided data's formatting/encoding does not match that which was
  -- expected. This error is an indication that one could attempt to
  -- deserialise the input again, but instead expecting a different format.

-- | Deserialise an input of some type that is formatted in some way.
deserialiseInput
  :: forall a. SerialiseAsCBOR a
  => SerialiseAsRawBytes a
  => AsType a
  -> NonEmpty (InputFormat a)
  -> ByteString
  -> Either InputDecodeError a
deserialiseInput asType acceptedFormats inputBs =
    go (NE.toList acceptedFormats)
  where
    inputText :: Text
    inputText = Text.decodeUtf8 inputBs

    go :: [InputFormat a] -> Either InputDecodeError a
    go [] = Left InputInvalidError
    go (kf:kfs) =
      let res =
            case kf of
              InputFormatBech32 -> deserialiseBech32
              InputFormatHex -> deserialiseHex
              InputFormatTextEnvelope -> deserialiseTextEnvelope
      in case res of
        DeserialiseInputSuccess a -> Right a
        DeserialiseInputError err -> Left err
        DeserialiseInputErrorFormatMismatch -> go kfs

    deserialiseTextEnvelope :: HasTextEnvelope a => DeserialiseInputResult a
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError a
          textEnvRes =
            deserialiseFromTextEnvelopeCBOR asType
              =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
      case textEnvRes of
        Right res -> DeserialiseInputSuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@TextEnvelopeTypeError{} ->
          DeserialiseInputError (InputTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseInputErrorFormatMismatch

    deserialiseBech32 :: SerialiseAsBech32 a => DeserialiseInputResult a
    deserialiseBech32 =
      case deserialiseFromBech32 asType inputText of
        Right res -> DeserialiseInputSuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseInputError $ InputBech32DecodeError err

    deserialiseHex :: DeserialiseInputResult a
    deserialiseHex
      | isValidHex inputBs =
          case deserialiseFromRawBytesHex asType inputBs of
            Left _ -> DeserialiseInputError InputInvalidError
            Right x -> DeserialiseInputSuccess x
      | otherwise = DeserialiseInputErrorFormatMismatch

    isValidHex :: ByteString -> Bool
    isValidHex x =
      all (`elem` hexAlpha) (toLower <$> BSC.unpack x)
        && even (BSC.length x)
      where
        hexAlpha :: [Char]
        hexAlpha = "0123456789abcdef"

-- | Deserialise an input of some type that is formatted in some way.
--
-- The provided 'ByteString' can either be Bech32-encoded or in the text
-- envelope format.
deserialiseInputAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 SerialiseAsRawBytes b]
  -> [FromSomeType HasTextEnvelope SerialiseAsCBOR b]
  -> ByteString
  -> Either InputDecodeError b
deserialiseInputAnyOf bech32Types textEnvTypes inputBs =
    case deserialiseBech32 `orTry` deserialiseTextEnvelope of
      DeserialiseInputSuccess res -> Right res
      DeserialiseInputError err -> Left err
      DeserialiseInputErrorFormatMismatch -> Left InputInvalidError
  where
    inputText :: Text
    inputText = Text.decodeUtf8 inputBs

    orTry
      :: DeserialiseInputResult b
      -> DeserialiseInputResult b
      -> DeserialiseInputResult b
    orTry x y =
      case x of
        DeserialiseInputSuccess _ -> x
        DeserialiseInputError _ -> x
        DeserialiseInputErrorFormatMismatch -> y

    deserialiseTextEnvelope :: DeserialiseInputResult b
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError b
          textEnvRes =
            deserialiseFromTextEnvelopeAnyOfCBOR textEnvTypes
              =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
      case textEnvRes of
        Right res -> DeserialiseInputSuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@TextEnvelopeTypeError{} ->
          DeserialiseInputError (InputTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseInputErrorFormatMismatch

    deserialiseBech32 :: DeserialiseInputResult b
    deserialiseBech32 =
      case deserialiseAnyOfFromBech32 bech32Types inputText of
        Right res -> DeserialiseInputSuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseInputError $ InputBech32DecodeError err

data SomeAddressVerificationKey
  = AByronVerificationKey           (VerificationKey ByronKey)
  | APaymentVerificationKey         (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey     (VerificationKey GenesisUTxOKey)
  | AKesVerificationKey             (VerificationKey KesKey)
  | AVrfVerificationKey             (VerificationKey VrfKey)
  | AStakeVerificationKey           (VerificationKey StakeKey)
  | AStakeExtendedVerificationKey   (VerificationKey StakeExtendedKey)
  deriving (Show)


deserialiseAnyVerificationKey
  :: ByteString -> Either InputDecodeError SomeAddressVerificationKey
deserialiseAnyVerificationKey bs =
  case deserialiseAnyVerificationKeyBech32 bs of
    Right vk -> Right vk
    Left _e ->
      case deserialiseAnyVerificationKeyTextEnvelope bs of
        Right vk -> Right vk
        Left _e -> Left InputInvalidError

deserialiseAnyVerificationKeyBech32
  :: ByteString -> Either Bech32DecodeError SomeAddressVerificationKey
deserialiseAnyVerificationKeyBech32 =
  deserialiseAnyOfFromBech32 allBech32VerKey . Text.decodeUtf8
 where
  allBech32VerKey
    :: [FromSomeType SerialiseAsBech32 SerialiseAsRawBytes SomeAddressVerificationKey]
  allBech32VerKey =
    [ FromSomeType (AsVerificationKey AsPaymentKey) APaymentVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentExtendedKey) APaymentExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsKesKey) AKesVerificationKey
    , FromSomeType (AsVerificationKey AsVrfKey) AVrfVerificationKey
    , FromSomeType (AsVerificationKey AsStakeKey) AStakeVerificationKey
    , FromSomeType (AsVerificationKey AsStakeExtendedKey) AStakeExtendedVerificationKey
    ]

deserialiseAnyVerificationKeyTextEnvelope
  :: ByteString -> Either TextEnvelopeError SomeAddressVerificationKey
deserialiseAnyVerificationKeyTextEnvelope bs =
  deserialiseFromTextEnvelopeAnyOfCBOR allTextEnvelopeCBOR
     =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' bs)
 where
  -- Therefore we must collect errors or have a separate function
  -- for textEnvelope Bech32 encoded things
  _allTextEnvelopBech32 =
    error "TODO: We want to also wrap the bech32 format in the TextEnvelope"
  allTextEnvelopeCBOR
    :: [FromSomeType HasTextEnvelope SerialiseAsCBOR SomeAddressVerificationKey]
  allTextEnvelopeCBOR =
    [ FromSomeType (AsVerificationKey AsByronKey) AByronVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentKey) APaymentVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentExtendedKey) APaymentExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsGenesisUTxOKey) AGenesisUTxOVerificationKey
    ]

{-
deserialiseInputAnyVerificationKey :: ByteString -> Either InputDecodeError b
deserialiseInputAnyVerificationKey = error ""
 where
    bech32Types
      :: [FromSomeType SerialiseAsBech32 SerialiseAsRawBytes SomeAddressVerificationKey]
    bech32Types =
      [ FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      ]
    textEnvTypes
      :: [FromSomeType HasTextEnvelope SerialiseAsCBOR SomeAddressVerificationKey]
    textEnvTypes =
      [ FromSomeType (AsVerificationKey AsByronKey)
                     AByronVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                     AGenesisUTxOVerificationKey
      ]

      -}
