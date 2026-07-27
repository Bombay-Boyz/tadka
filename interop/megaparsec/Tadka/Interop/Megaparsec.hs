-- | One-directional adapter: megaparsec parse-error positions → tadka 'Span'
-- (spec Phase 10). megaparsec carries a stream 'errorOffset' (a character
-- offset), the natural bridge to tadka's offset-based spans.
-- Minimum supported: @megaparsec >= 9.0@.
--
-- No compatibility guarantee.
module Tadka.Interop.Megaparsec
  ( spanFromError
  ) where

import           Text.Megaparsec.Error (ParseError, errorOffset)

import           Tadka                 (Span, SpanBuildError, mkSpan)

-- | A 'Span' of the given character length starting at the error's offset
-- (length 0 for a point span).
spanFromError :: Int -> ParseError s e -> Either SpanBuildError Span
spanFromError len e = mkSpan (errorOffset e) len
