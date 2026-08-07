{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Primitive validated types and their smart constructors (vision §2 partial,
-- §4, §5 partial; spec Phase 1).
--
-- Every type here follows the same discipline: the raw newtype/record
-- constructor is /not/ exported, so the only way to obtain a value is through a
-- smart constructor that has already enforced the type's invariant. Downstream
-- code therefore never re-checks these invariants ("make illegal states
-- unrepresentable").
--
-- No compatibility guarantee (internal module). The public, stable names are
-- re-exported from "Tadka"; the two @unsafe*@ constructors are re-exported from
-- "Tadka.Internal" for splice-time use only and never reach "Tadka".
module Tadka.Internal.Types
  ( -- * Offsets and lengths
    Offset
  , unOffset
  , mkOffset
  , OffsetError (..)
  , Length
  , unLength
  , mkLength
  , LengthError (..)
    -- * Named source
  , NamedSource
  , sourceName
  , sourceText
  , mkNamedSource
  , SourceError (..)
    -- * Diagnostic codes
  , DiagnosticCode
  , unDiagnosticCode
  , mkDiagnosticCode
  , CodeError (..)
    -- * URLs
  , Url
  , unUrl
  , mkUrl
  , UrlError (..)
    -- * Severity
  , Severity (..)
  , SeverityLabels (..)
  , severityLabels
  , severityJsonTag
    -- * Diagnostic identity
  , DiagnosticId
  , unDiagnosticId
  , mkDiagnosticId
    -- * Unsafe constructors (internal; no validation)
  , unsafeDiagnosticCode
  , unsafeUrl
  ) where

import           Data.Char (isAsciiLower, isDigit)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI (parseAbsoluteURI)

-- ---------------------------------------------------------------------------
-- Offset / Length
-- ---------------------------------------------------------------------------

-- | A non-negative byte/char offset into a source. Construct via 'mkOffset'.
newtype Offset = Offset Int
  deriving (Eq, Ord, Show)

unOffset :: Offset -> Int
unOffset (Offset n) = n

-- | Why 'mkOffset' rejected an input.
newtype OffsetError = NegativeOffset Int
  deriving (Eq, Show)

-- | Build an 'Offset', rejecting negatives.
mkOffset :: Int -> Either OffsetError Offset
mkOffset n
  | n < 0     = Left (NegativeOffset n)
  | otherwise = Right (Offset n)

-- | A non-negative span length. Zero is a valid point span. Construct via
-- 'mkLength'.
newtype Length = Length Int
  deriving (Eq, Ord, Show)

unLength :: Length -> Int
unLength (Length n) = n

-- | Why 'mkLength' rejected an input.
newtype LengthError = NegativeLength Int
  deriving (Eq, Show)

-- | Build a 'Length', rejecting negatives (zero is allowed).
mkLength :: Int -> Either LengthError Length
mkLength n
  | n < 0     = Left (NegativeLength n)
  | otherwise = Right (Length n)

-- ---------------------------------------------------------------------------
-- NamedSource
-- ---------------------------------------------------------------------------

-- | A named blob of source text. The constructor is not exported; the
-- 'sourceName' / 'sourceText' selectors are read-only. Construct via
-- 'mkNamedSource'.
data NamedSource = NamedSource
  { sourceName :: !Text
  , sourceText :: !Text
  }
  deriving (Eq, Show)

-- | Why 'mkNamedSource' rejected an input.
data SourceError = EmptySourceName
  deriving (Eq, Show)

-- | Build a 'NamedSource'. Rejects an empty name; empty /content/ is
-- legitimate (an empty file is a real thing to point at).
mkNamedSource :: Text -> Text -> Either SourceError NamedSource
mkNamedSource name txt
  | T.null name = Left EmptySourceName
  | otherwise   = Right (NamedSource name txt)

-- ---------------------------------------------------------------------------
-- DiagnosticCode
-- ---------------------------------------------------------------------------

-- | A validated diagnostic code such as @tadka::E0001@. Construct via
-- 'mkDiagnosticCode' (or, internally only, 'unsafeDiagnosticCode').
newtype DiagnosticCode = DiagnosticCode Text
  deriving (Eq, Show)

unDiagnosticCode :: DiagnosticCode -> Text
unDiagnosticCode (DiagnosticCode t) = t

-- | Why 'mkDiagnosticCode' rejected an input.
data CodeError
  = EmptyCode
  | MalformedCode Text
  deriving (Eq, Show)

-- | Build a 'DiagnosticCode', enforcing the grammar
-- @^[a-z][a-z0-9_]*::E[0-9]{4,}$@.
mkDiagnosticCode :: Text -> Either CodeError DiagnosticCode
mkDiagnosticCode t
  | T.null t              = Left EmptyCode
  | matchesCodeGrammar t  = Right (DiagnosticCode t)
  | otherwise             = Left (MalformedCode t)

-- | @^[a-z][a-z0-9_]*::E[0-9]{4,}$@ without a regex dependency.
matchesCodeGrammar :: Text -> Bool
matchesCodeGrammar t =
  case T.stripPrefix "::" rest of
    Just body -> validNamespace ns && validBody body
    Nothing   -> False
  where
    (ns, rest) = T.breakOn "::" t

    validNamespace n = case T.uncons n of
      Just (c0, cs) ->
        isAsciiLower c0
          && T.all (\c -> isAsciiLower c || isDigit c || c == '_') cs
      Nothing -> False

    validBody b = case T.uncons b of
      Just ('E', ds) -> T.length ds >= 4 && T.all isDigit ds
      _              -> False

-- | Internal only: wrap already-validated text with no checks. Used by the
-- derive macro after splice-time validation. Never reachable from
-- "Tadka".
unsafeDiagnosticCode :: Text -> DiagnosticCode
unsafeDiagnosticCode = DiagnosticCode

-- ---------------------------------------------------------------------------
-- Url
-- ---------------------------------------------------------------------------

-- | A validated absolute URL. Construct via 'mkUrl' (or, internally only,
-- 'unsafeUrl').
newtype Url = Url Text
  deriving (Eq, Show)

unUrl :: Url -> Text
unUrl (Url t) = t

-- | Why 'mkUrl' rejected an input.
data UrlError
  = EmptyUrl
  | NotAbsoluteUri Text
  deriving (Eq, Show)

-- | Build a 'Url', requiring it to parse as an absolute URI.
mkUrl :: Text -> Either UrlError Url
mkUrl t
  | T.null t                            = Left EmptyUrl
  | isJust (parseAbsoluteURI (T.unpack t)) = Right (Url t)
  | otherwise                           = Left (NotAbsoluteUri t)

-- | Internal only: wrap already-validated text with no checks.
unsafeUrl :: Text -> Url
unsafeUrl = Url

-- ---------------------------------------------------------------------------
-- Severity
-- ---------------------------------------------------------------------------

-- | Diagnostic severity. Ordered 'SevAdvice' < 'SevWarning' < 'SevError'.
data Severity
  = SevAdvice
  | SevWarning
  | SevError
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The display strings for a severity, kept in one place so the graphical
-- and narratable handlers read from a single source of
-- truth rather than scattered literals. The JSON handler uses
-- 'severityJsonTag' instead.
data SeverityLabels = SeverityLabels
  { severityGraphicalHeader  :: !Text  -- ^ e.g. @"error:"@
  , severityNarratablePrefix :: !Text  -- ^ e.g. @"Error,"@
  }
  deriving (Eq, Show)

-- | The single mapping from 'Severity' to its human-facing display strings.
severityLabels :: Severity -> SeverityLabels
severityLabels = \case
  SevAdvice  -> SeverityLabels "advice:"  "Advice,"
  SevWarning -> SeverityLabels "warning:" "Warning,"
  SevError   -> SeverityLabels "error:"   "Error,"

-- | The bare lowercase JSON tag for a 'Severity'.
severityJsonTag :: Severity -> Text
severityJsonTag = \case
  SevAdvice  -> "advice"
  SevWarning -> "warning"
  SevError   -> "error"

-- ---------------------------------------------------------------------------
-- DiagnosticId
-- ---------------------------------------------------------------------------

-- | An opaque identity key used only to detect cycles in @related@ chains
-- . Any 'Text' is a valid id, so 'mkDiagnosticId' is total; the
-- constructor stays hidden so an invariant could be added later without an API
-- break.
newtype DiagnosticId = DiagnosticId Text
  deriving (Eq, Ord, Show)

unDiagnosticId :: DiagnosticId -> Text
unDiagnosticId (DiagnosticId t) = t

-- | Build a 'DiagnosticId'. Total: any 'Text' is a valid comparison key.
mkDiagnosticId :: Text -> DiagnosticId
mkDiagnosticId = DiagnosticId
