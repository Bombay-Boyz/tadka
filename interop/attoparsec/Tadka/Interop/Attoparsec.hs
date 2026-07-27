-- | One-directional adapter: attoparsec failure positions → tadka
-- 'Offset'\/'Span' (spec Phase 10). attoparsec is position-agnostic (it reports
-- no line/column), so position is recovered as the number of characters
-- consumed before the failure: @length original - length remaining@.
-- Minimum supported: @attoparsec >= 0.14@.
--
-- No compatibility guarantee.
module Tadka.Interop.Attoparsec
  ( consumedOffset
  , spanFromConsumed
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Tadka     (Span, SpanBuildError, mkSpan)

-- | Characters consumed before a failure, given the original input and the
-- unconsumed remainder (from an attoparsec @Fail@\/@Done@ result).
consumedOffset :: Text -> Text -> Int
consumedOffset orig remaining = T.length orig - T.length remaining

-- | A 'Span' of the given length at the consumed offset.
spanFromConsumed :: Text -> Text -> Int -> Either SpanBuildError Span
spanFromConsumed orig remaining len = mkSpan (consumedOffset orig remaining) len
