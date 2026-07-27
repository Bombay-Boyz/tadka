{-# LANGUAGE OverloadedStrings #-}

-- | JSON report handler (vision §7, spec Phase 7): the machine-readable
-- renderer. @Output 'TJson = Aeson.Value@ is produced only here, from a
-- dedicated 'DiagnosticDTO' — never by @deriving ToJSON@ on a
-- @Diagnostic@-bearing type. @ToJSON@ only in v1; a reviewed @FromJSON@ decode
-- path is deferred.
--
-- The DTO carries an explicit @"stale"@ flag per label (from
-- 'Tadka.Internal.Context.LabelState', not inferred from absence) and
-- @"truncated"@\/@"cycleOmitted"@ flags per level (from the Phase 3 walk's
-- 'TerminationReason'), so a machine consumer sees every state the graphical
-- and narratable handlers show visually.
--
-- No compatibility guarantee.
module Tadka.Internal.Renderer.Json
  ( JsonOptions (..)
  , DiagnosticDTO (..)
  , LabelDTO (..)
  , CauseDTO (..)
  , toDTO
  , renderJson
  ) where

import           Data.Aeson                (ToJSON (..), Value, object, (.=))
import qualified Data.List.NonEmpty        as NE
import           Data.Text                 (Text)
import           Numeric.Natural           (Natural)
import           Prettyprinter             (Doc, LayoutOptions (..),
                                            PageWidth (Unbounded), layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)

import           Tadka.Internal.Ann        (Ann)
import           Tadka.Internal.Context    (Context (..), LabelKind (..),
                                            LabelState (..), Labeled (..))
import           Tadka.Internal.Diagnostic (Diagnostic (..), SomeDiagnostic (..))
import           Tadka.Internal.Related    (RelatedTree (..), TerminationReason (..),
                                            walkCauses, walkRelated)
import           Tadka.Internal.Span       (LineCol (..), resolvedStart, spanLength)
import           Tadka.Internal.Types      (severityJsonTag, unDiagnosticCode,
                                            unLength, unUrl)

-- | Resolved settings for the JSON handler (populated only by @selectRenderer@).
newtype JsonOptions = JsonOptions
  { joRelatedDepth :: Natural }
  deriving (Eq, Show)

-- | One label in the DTO. @line@\/@column@\/@length@ are 'Nothing' (JSON null)
-- when the label is stale; @stale@ always reflects
-- 'Tadka.Internal.Context.LabelState' explicitly.
data LabelDTO = LabelDTO
  { ldLine    :: Maybe Int
  , ldColumn  :: Maybe Int
  , ldLength  :: Maybe Int
  , ldText    :: Maybe Text
  , ldPrimary :: Bool
  , ldStale   :: Bool
  }
  deriving (Eq, Show)

-- | The machine-readable diagnostic. @truncated@\/@cycleOmitted@ record whether
-- related entries at this level were dropped at the depth limit or as a cycle.
data DiagnosticDTO = DiagnosticDTO
  { dtoCode         :: Maybe Text
  , dtoSeverity     :: Text
  , dtoMessage      :: Text
  , dtoLabels       :: [LabelDTO]
  , dtoHelp         :: Maybe Text
  , dtoUrl          :: Maybe Text
  , dtoRelated      :: [DiagnosticDTO]
  , dtoCauses       :: [CauseDTO]
  , dtoTruncated    :: Bool
  , dtoCycleOmitted :: Bool
  }
  deriving (Eq, Show)

instance ToJSON LabelDTO where
  toJSON l = object
    [ "line"    .= ldLine l
    , "column"  .= ldColumn l
    , "length"  .= ldLength l
    , "text"    .= ldText l
    , "primary" .= ldPrimary l
    , "stale"   .= ldStale l
    ]

-- | A cause is rendered as lightweight provenance (code + message), matching
-- the summary treatment the graphical and narratable handlers give the chain.
data CauseDTO = CauseDTO
  { causeCode    :: Maybe Text
  , causeMessage :: Text
  }
  deriving (Eq, Show)

instance ToJSON CauseDTO where
  toJSON c = object [ "code" .= causeCode c, "message" .= causeMessage c ]

instance ToJSON DiagnosticDTO where
  toJSON d = object
    [ "code"         .= dtoCode d
    , "severity"     .= dtoSeverity d
    , "message"      .= dtoMessage d
    , "labels"       .= dtoLabels d
    , "help"         .= dtoHelp d
    , "url"          .= dtoUrl d
    , "related"      .= dtoRelated d
    , "causes"       .= dtoCauses d
    , "truncated"    .= dtoTruncated d
    , "cycleOmitted" .= dtoCycleOmitted d
    ]

-- | Convert a Phase 3 walk tree into a DTO. Cycle-marker children become the
-- @cycleOmitted@ flag (not nested entries); depth-truncation becomes
-- @truncated@ on the node whose children were cut.
toDTO :: RelatedTree -> DiagnosticDTO
toDTO (RelatedTree (SomeDiagnostic e) children term) = DiagnosticDTO
  { dtoCode         = unDiagnosticCode <$> code e
  , dtoSeverity     = severityJsonTag (severity e)
  , dtoMessage      = docToText (message e)
  , dtoLabels       = labelsDTO (context e)
  , dtoHelp         = docToText <$> help e
  , dtoUrl          = unUrl <$> url e
  , dtoRelated      = [ toDTO c | c <- children, not (isCycle c) ]
  , dtoCauses       = []
  , dtoTruncated    = term == DepthTruncated
  , dtoCycleOmitted = any isCycle children
  }
  where isCycle (RelatedTree _ _ t) = t == CycleOmitted

labelsDTO :: Context -> [LabelDTO]
labelsDTO NoContext          = []
labelsDTO (HasLabels _ lbls) = map toLabel (NE.toList lbls)
  where
    toLabel (Labeled (LabelOk rs) k txt) = LabelDTO
      { ldLine    = Just (lcLine (resolvedStart rs))
      , ldColumn  = Just (lcColumn (resolvedStart rs))
      , ldLength  = Just (unLength (spanLength rs))
      , ldText    = docToText <$> txt
      , ldPrimary = k == Primary
      , ldStale   = False
      }
    toLabel (Labeled (LabelStale _) k txt) = LabelDTO
      { ldLine = Nothing, ldColumn = Nothing, ldLength = Nothing
      , ldText = docToText <$> txt, ldPrimary = k == Primary, ldStale = True
      }

renderJson :: Diagnostic e => JsonOptions -> e -> Value
renderJson opts e =
  toJSON (dto { dtoCauses = map causeDTO (walkCauses depth root) })
  where
    root  = SomeDiagnostic e
    depth = joRelatedDepth opts
    dto   = toDTO (walkRelated depth root)

causeDTO :: SomeDiagnostic -> CauseDTO
causeDTO (SomeDiagnostic e) = CauseDTO (unDiagnosticCode <$> code e) (docToText (message e))

-- Plain-text rendering of an annotated document (annotations discarded); JSON
-- string values carry the raw message\/label\/help text.
docToText :: Doc Ann -> Text
docToText = renderStrict . layoutPretty (LayoutOptions Unbounded)
