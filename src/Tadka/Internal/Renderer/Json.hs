{-# LANGUAGE OverloadedStrings #-}

-- | JSON report handler: the machine-readable
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
import           Data.Char                 (isControl)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Numeric.Natural           (Natural)
import           Prettyprinter             (Doc, LayoutOptions (..),
                                            PageWidth (Unbounded), layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)

import           Tadka.Internal.Ann        (Ann)
import           Tadka.Internal.Context    (Context, LabelKind (..),
                                            LabelState (..), Labeled (..),
                                            SourceGroup (..), contextSourceGroups)
import           Tadka.Internal.Diagnostic (Diagnostic (..), SomeDiagnostic (..))
import           Tadka.Internal.Related    (RelatedTree (..), TerminationReason (..),
                                            walkCauses, walkRelated)
import           Tadka.Internal.Span       (LineCol (..), resolvedStart, spanLength)
import           Tadka.Internal.Types      (severityJsonTag, sourceName, unDiagnosticCode,
                                            unLength, unUrl)

-- | Resolved settings for the JSON handler (populated only by @selectRenderer@).
newtype JsonOptions = JsonOptions
  { joRelatedDepth :: Natural }
  deriving (Eq, Show)

-- | One label in the DTO. @line@\/@column@\/@length@ are 'Nothing' (JSON null)
-- when the label is stale; @stale@ always reflects
-- 'Tadka.Internal.Context.LabelState' explicitly. @file@ is always present
-- : it names the source the label belongs to, even when the label
-- is stale, so a machine consumer never has to infer which file a label was
-- meant for from position alone.
data LabelDTO = LabelDTO
  { ldFile    :: Text
  , ldLine    :: Maybe Int
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
    [ "file"    .= ldFile l
    , "line"    .= ldLine l
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
-- @truncated@ on the node whose children were cut. The same 'depth' budget
-- used to build the 'related' tree also bounds each node's own 'causes'
-- chain — one recursive definition, applied uniformly to the root and to
-- every related diagnostic, so a node's causes never depend on whether it
-- happens to be the one the caller started from.
toDTO :: Natural -> RelatedTree -> DiagnosticDTO
toDTO depth (RelatedTree sd@(SomeDiagnostic e) children term) = DiagnosticDTO
  { dtoCode         = unDiagnosticCode <$> code e
  , dtoSeverity     = severityJsonTag (severity e)
  , dtoMessage      = docToText (message e)
  , dtoLabels       = labelsDTO (context e)
  , dtoHelp         = docToText <$> help e
  , dtoUrl          = unUrl <$> url e
  , dtoRelated      = [ toDTO depth c | c <- children, not (isCycle c) ]
  , dtoCauses       = map causeDTO (walkCauses depth sd)
  , dtoTruncated    = term == DepthTruncated
  , dtoCycleOmitted = any isCycle children
  }
  where isCycle (RelatedTree _ _ t) = t == CycleOmitted

labelsDTO :: Context -> [LabelDTO]
labelsDTO = concatMap groupLabels . contextSourceGroups
  where
    groupLabels :: SourceGroup -> [LabelDTO]
    groupLabels (SourceGroup src lbls) = map (toLabel (sourceName src)) (NE.toList lbls)

    toLabel :: Text -> Labeled LabelState -> LabelDTO
    toLabel file (Labeled (LabelOk rs) k txt) = LabelDTO
      { ldFile    = file
      , ldLine    = Just (lcLine (resolvedStart rs))
      , ldColumn  = Just (lcColumn (resolvedStart rs))
      , ldLength  = Just (unLength (spanLength rs))
      , ldText    = docToText <$> txt
      , ldPrimary = k == Primary
      , ldStale   = False
      }
    toLabel file (Labeled (LabelStale _) k txt) = LabelDTO
      { ldFile = file, ldLine = Nothing, ldColumn = Nothing, ldLength = Nothing
      , ldText = docToText <$> txt, ldPrimary = k == Primary, ldStale = True
      }

renderJson :: Diagnostic e => JsonOptions -> e -> Value
renderJson opts e = toJSON (toDTO depth (walkRelated depth root))
  where
    root  = SomeDiagnostic e
    depth = joRelatedDepth opts

causeDTO :: SomeDiagnostic -> CauseDTO
causeDTO (SomeDiagnostic e) = CauseDTO (unDiagnosticCode <$> code e) (docToText (message e))

-- Plain-text rendering of an annotated document (annotations discarded); JSON
-- string values carry the raw message\/label\/help text.
docToText :: Doc Ann -> Text
docToText = T.map (\c -> if isControl c then ' ' else c)
          . renderStrict . layoutPretty (LayoutOptions Unbounded)
