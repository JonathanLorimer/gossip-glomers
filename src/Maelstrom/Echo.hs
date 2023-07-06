module Maelstrom.Echo where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID
import Deriving.Aeson
import GHC.Natural
import Text.Read (readMaybe)
import Data.Aeson.KeyMap
import Data.ByteString (ByteString)
import Utils.Either (onLeftA)
import Control.Exception (throwIO, Exception)

newtype Client = Client {unClient :: Natural}
  deriving newtype (Show)

instance FromJSON Client where
  parseJSON =
    withText "Client" $
      ( \case
          'c' : n -> case readMaybe n of
            Nothing -> fail "Couldn't read a number proceeding \"c\" when parsing a Client"
            Just nat -> pure $ Client nat
          _ -> fail "Malformed Client when parsing Json, expected a \"c\" prefix"
      )
        . T.unpack

instance ToJSON Client where
  toJSON (Client nat) = String $ "c" <> T.pack (show nat)

newtype Node = Node {unNode :: Natural}
  deriving newtype (Show)

instance FromJSON Node where
  parseJSON =
    withText "Node" $
      ( \case
          'n' : n -> case readMaybe n of
            Nothing -> fail "Couldn't read a number proceeding \"c\" when parsing a Client"
            Just nat -> pure $ Node nat
          _ -> fail "Malformed Client when parsing Json, expected a \"c\" prefix"
      )
        . T.unpack

instance ToJSON Node where
  toJSON (Node nat) = String $ "n" <> T.pack (show nat)

data Envelope = Envelope
  { src :: Client
  , dest :: Node
  , body :: Message
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data Message = Echo EchoMsg | Ok OkMsg
  deriving stock (Show)

instance FromJSON Message where
  parseJSON v =
    withObject "Message" (\obj -> do
      (ty :: String) <- obj .: "type"
      case ty of
        "echo" -> Echo <$> parseJSON v
        "echo_ok" -> Ok <$> parseJSON v
        _ -> fail "Expected one of: \"echo\", \"echo_ok\" for Message type"
    ) v

instance ToJSON Message where
  toJSON (Echo echoMsg) = case toJSON echoMsg of 
                            Object km -> object $ [ "type" .= ("echo" :: String) ] <> toList km
                            _ -> error "Invariant Violated: EchoMsg should always serialize to an Object"
  toJSON (Ok okMsg) = case toJSON okMsg of 
                            Object km -> object $ [ "type" .= ("echo_ok" :: String) ] <> toList km
                            _ -> error "Invariant Violated: EchoMsg should always serialize to an Object"

data EchoMsg = EchoMsg
  { echoMsgMsgId :: UUID
  , echoMsgEcho :: Text
  } 
  deriving stock (Generic, Show)
  deriving (FromJSON) via (CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "echoMsg", CamelToSnake]] EchoMsg)
  deriving (ToJSON) via (CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "echoMsg", CamelToSnake]] EchoMsg)

data OkMsg = OkMsg
  { okMsgMsgId :: UUID
  , okMsgInReplyTo :: UUID
  , okMsgEcho :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via (CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "okMsg", CamelToSnake]] OkMsg)
  deriving (ToJSON) via (CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "okMsg", CamelToSnake]] OkMsg)

newtype JSONDecodingError = JSONDecodingError String
  deriving stock (Show)

instance Exception JSONDecodingError

echoHandler :: ByteString -> IO ()
echoHandler bs = do
  envelope <- onLeftA (eitherDecodeStrict @Envelope bs) (throwIO . JSONDecodingError)
  case envelope.body of
    Ok msg -> print msg
    Echo msg -> print msg
