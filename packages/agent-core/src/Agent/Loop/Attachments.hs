-- | Provider-neutral user-turn attachment types.
--
-- These types sit in their own leaf module (no agent-core dependencies) so
-- that modules needing only attachment types — UI input state, verification
-- components — do not have to link the full agent loop.
module Agent.Loop.Attachments
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnAttachment(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)

-- | Image bytes attached to a user turn (PNG/JPEG/…).
data ImageAttachment = ImageAttachment
    { imageMime :: !Text
    , imageBytes :: !ByteString
    } deriving (Eq)

instance Show ImageAttachment where
    show image =
        "ImageAttachment { imageMime = " <> show image.imageMime
            <> ", imageBytes = <redacted>"
            <> ", imageByteLength = " <> show (ByteString.length image.imageBytes)
            <> " }"

-- | File bytes attached to a user turn. Providers that cannot ingest files
-- natively should fall back to a local path or text summary.
data FileAttachment = FileAttachment
    { fileName :: !(Maybe Text)
    , fileMime :: !Text
    , fileBytes :: !ByteString
    } deriving (Eq)

instance Show FileAttachment where
    show file =
        "FileAttachment { fileName = " <> show file.fileName
            <> ", fileMime = " <> show file.fileMime
            <> ", fileBytes = <redacted>"
            <> ", fileByteLength = " <> show (ByteString.length file.fileBytes)
            <> " }"

-- | A provider-neutral user attachment, retained in source order.
data TurnAttachment
    = ImageAttachmentItem !ImageAttachment
    | FileAttachmentItem !FileAttachment
    deriving (Eq, Show)
