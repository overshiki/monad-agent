-- | Provider-neutral messages exchanged between a root agent and subagents.
--
-- OpenAI collaboration tools may return encrypted message fields. The opaque
-- payload must stay typed until the OpenAI adapter emits an @agent_message@
-- item with an @encrypted_content@ part; treating it as ordinary user text
-- prevents the child model from decrypting it.
module Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , InterAgentMessageType(..)
    , encryptedInterAgentContent
    , interAgentMessagePayload
    , plainInterAgentContent
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-core-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- The confidentiality contract: the log-safe 'show' of encrypted content is
-- provably constant ('redacted'), so the payload cannot leak through it.
-- No axioms are needed; the property is structural.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

{-@ data InterAgentMessageContent
      = PlainInterAgentContent Text
      | EncryptedInterAgentContent Text
@-}

-- | The exact string emitted for encrypted content. Reflected so the
-- contract can name it.
{-@ reflect redacted @-}
redacted :: String
redacted = "EncryptedInterAgentContent <redacted>"

{-@ reflect isEncryptedContent @-}
isEncryptedContent :: InterAgentMessageContent -> Bool
isEncryptedContent (EncryptedInterAgentContent _) = True
isEncryptedContent _ = False

data InterAgentMessageType
    = NewTaskMessage
    | QueuedMessage
    | FollowUpMessage
    deriving (Eq, Show)

data InterAgentMessageContent
    = PlainInterAgentContent !Text
    | EncryptedInterAgentContent !Text
    deriving (Eq)

-- | Log-safe rendering of message content. For encrypted content the result
-- is exactly 'redacted' — the payload cannot appear in it.
{-@ showInterAgentMessageContent :: c:InterAgentMessageContent
      -> {v:[Char] | isEncryptedContent c => v == redacted} @-}
showInterAgentMessageContent :: InterAgentMessageContent -> String
showInterAgentMessageContent = \case
    PlainInterAgentContent text ->
        "PlainInterAgentContent " <> show text
    EncryptedInterAgentContent _ ->
        redacted

instance Show InterAgentMessageContent where
    show = showInterAgentMessageContent

data InterAgentMessage = InterAgentMessage
    { messageAuthor :: !Text
    , messageRecipient :: !Text
    , messageType :: !InterAgentMessageType
    , messageContent :: !InterAgentMessageContent
    } deriving (Eq)

instance Show InterAgentMessage where
    show (InterAgentMessage author recipient messageType content) =
        "InterAgentMessage { messageAuthor = " <> show author
            <> ", messageRecipient = " <> show recipient
            <> ", messageType = " <> show messageType
            <> ", messageContent = " <> show content
            <> " }"

plainInterAgentContent :: Text -> InterAgentMessageContent
plainInterAgentContent = PlainInterAgentContent

encryptedInterAgentContent :: Text -> InterAgentMessageContent
encryptedInterAgentContent = EncryptedInterAgentContent

renderInterAgentMessageHeader :: InterAgentMessage -> Text
renderInterAgentMessageHeader (InterAgentMessage author recipient messageType _) =
    Text.unlines $
        [ "Message Type: " <> messageTypeText messageType
        ]
        <> taskLine
        <> [ "Sender: " <> author
           , "Payload:"
           ]
  where
    taskLine = case messageType of
        NewTaskMessage -> ["Task name: " <> recipient]
        QueuedMessage -> []
        FollowUpMessage -> []

renderInterAgentMessage :: InterAgentMessage -> Text
renderInterAgentMessage message =
    renderInterAgentMessageHeader message <> interAgentMessagePayload message

interAgentMessagePayload :: InterAgentMessage -> Text
interAgentMessagePayload (InterAgentMessage _ _ _ content) = case content of
    PlainInterAgentContent text -> text
    -- Non-OpenAI transports do not currently advertise encrypted
    -- collaboration fields. Preserve the token rather than dropping it if
    -- one nevertheless reaches this fallback.
    EncryptedInterAgentContent text -> text

messageTypeText :: InterAgentMessageType -> Text
messageTypeText = \case
    NewTaskMessage -> "NEW_TASK"
    QueuedMessage -> "MESSAGE"
    FollowUpMessage -> "FOLLOW_UP"
