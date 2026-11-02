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
  , jsonOptions
  , DiagnosticDTO (..)
  , LabelDTO (..)
  , CauseDTO (..)
  , currentSchemaVersion
  , toDTO
  , renderJson
  ) where

import           Control.Applicative       ((<|>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), Value (Object), object,
                                            withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap         as KM
import qualified Data.List.NonEmpty        as NE
import           Data.Char                 (isControl)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Numeric.Natural           (Natural)
import           Prettyprinter             (Doc, LayoutOptions (..),
                                            PageWidth (Unbounded), layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)

import           Tadka.Internal.Ann        (Ann)
import           Tadka.Internal.Config     (CauseDetail (..), Config, configCauseDetail,
                                            configRelatedDepth)
import           Tadka.Internal.Context    (Context, LabelKind (..),
                                            LabelState (..), Labeled (..),
                                            SourceGroup (..), contextSourceGroups)
import           Tadka.Internal.Diagnostic (Diagnostic (..), SomeDiagnostic (..))
import           Tadka.Internal.Related    (RelatedTree (..), TerminationReason (..),
                                            walkCauses, walkRelated)
import           Tadka.Internal.SourceCode (SourceCode (..))
import           Tadka.Internal.Span       (LineCol (..), resolvedStart, spanLength)
import           Tadka.Internal.Types      (severityJsonTag, unDiagnosticCode,
                                            unLength, unUrl)

-- | Resolved settings for the JSON handler (populated only by @selectRenderer@).
data JsonOptions = JsonOptions
  { joRelatedDepth :: Natural
  , joCauseDetail  :: CauseDetail
  }
  deriving (Eq, Show)

-- | The JSON handler's settings, read from a 'Config'. This is the one place
-- a 'JsonOptions' is built, so a new option is added here and nowhere else.
jsonOptions :: Config -> JsonOptions
jsonOptions cfg = JsonOptions
  { joRelatedDepth = configRelatedDepth cfg
  , joCauseDetail  = configCauseDetail cfg
  }

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

instance FromJSON LabelDTO where
  parseJSON = withObject "LabelDTO" $ \o -> LabelDTO
    <$> o .: "file"
    <*> o .: "line"
    <*> o .: "column"
    <*> o .: "length"
    <*> o .: "text"
    <*> o .: "primary"
    <*> o .: "stale"

-- | A diagnostic's own @diagnosticCause@ chain, one entry per cause. Under
-- 'CauseSummary' (the default) that is lightweight provenance (code +
-- message) — 'CauseDTOSummary'. Under 'CauseFull' it is the cause's own
-- full 'DiagnosticDTO', nested exactly like a 'dtoRelated' entry —
-- 'CauseDTOFull'. A single sum type, rather than always emitting the full
-- shape, keeps the default output exactly what it always was: no new keys
-- on a cause unless 'CauseFull' was asked for.
data CauseDTO
  = CauseDTOSummary (Maybe Text) Text
  | CauseDTOFull DiagnosticDTO
  deriving (Eq, Show)

instance ToJSON CauseDTO where
  toJSON (CauseDTOSummary c m) = object [ "code" .= c, "message" .= m ]
  toJSON (CauseDTOFull d)      = toJSON d

-- | Tries the full-report shape first (every 'DiagnosticDTO' field is
-- required, so this fails fast on a summary-shaped object), then falls
-- back to the summary shape. A value produced by this module's own
-- 'ToJSON' instance is therefore always decoded back to the same
-- constructor it was encoded from.
instance FromJSON CauseDTO where
  parseJSON v =
    (CauseDTOFull <$> parseJSON v)
      <|> withObject "CauseDTO" (\o -> CauseDTOSummary <$> o .: "code" <*> o .: "message") v

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

-- | The inverse of the 'ToJSON' instance above, field for field. Ignores an
-- extra @"schemaVersion"@ key gracefully (an ordinary unused-key skip, not
-- special-cased) — 'renderJson' merges that key into the /root/ object
-- only, so this instance decodes both a root-level and a nested
-- 'DiagnosticDTO' the same way.
instance FromJSON DiagnosticDTO where
  parseJSON = withObject "DiagnosticDTO" $ \o -> DiagnosticDTO
    <$> o .: "code"
    <*> o .: "severity"
    <*> o .: "message"
    <*> o .: "labels"
    <*> o .: "help"
    <*> o .: "url"
    <*> o .: "related"
    <*> o .: "causes"
    <*> o .: "truncated"
    <*> o .: "cycleOmitted"

-- | The JSON schema's own version, independent of the package version:
-- bumped only when the /shape/ of 'DiagnosticDTO'\/'LabelDTO'\/'CauseDTO'
-- changes in a way a consumer must handle (a field added, removed, or
-- retyped) — not on every release. 'renderJson' merges this into the root
-- object as @"schemaVersion"@, so a consumer that wants a stability
-- guarantee stronger than the README's prior advice ("pin the tadka
-- version") can assert on this field instead, independent of which tadka
-- release produced it.
currentSchemaVersion :: Int
currentSchemaVersion = 1

-- | Convert a Phase 3 walk tree into a DTO. Cycle-marker children become the
-- @cycleOmitted@ flag (not nested entries); depth-truncation becomes
-- @truncated@ on the node whose children were cut. The same 'depth' budget
-- used to build the 'related' tree also bounds each node's own 'causes'
-- chain — one recursive definition, applied uniformly to the root and to
-- every related diagnostic, so a node's causes never depend on whether it
-- happens to be the one the caller started from.
toDTO :: CauseDetail -> Natural -> RelatedTree -> DiagnosticDTO
toDTO cd depth = toDTOFuel cd depth depth

-- | 'toDTO' with an explicit @fuel@ bounding how many times full cause detail
-- may nest (each full cause spends one unit; see 'causeDTO'). It starts at the
-- related-depth limit, so a structurally cyclic diagnostic still terminates.
toDTOFuel :: CauseDetail -> Natural -> Natural -> RelatedTree -> DiagnosticDTO
toDTOFuel cd depth fuel (RelatedTree sd@(SomeDiagnostic e) children term) = DiagnosticDTO
  { dtoCode         = unDiagnosticCode <$> code e
  , dtoSeverity     = severityJsonTag (severity e)
  , dtoMessage      = docToText (message e)
  , dtoLabels       = labelsDTO (context e)
  , dtoHelp         = docToText <$> help e
  , dtoUrl          = unUrl <$> url e
  , dtoRelated      = [ toDTOFuel cd depth fuel c | c <- children, not (isCycle c) ]
  , dtoCauses       = map (causeDTO cd depth fuel) (walkCauses depth sd)
  , dtoTruncated    = term == DepthTruncated
  , dtoCycleOmitted = any isCycle children
  }
  where isCycle (RelatedTree _ _ t) = t == CycleOmitted

labelsDTO :: Context -> [LabelDTO]
labelsDTO = concatMap groupLabels . contextSourceGroups
  where
    groupLabels :: SourceGroup -> [LabelDTO]
    groupLabels (SourceGroup src lbls) = map (toLabel (scName src)) (NE.toList lbls)

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

-- | Render to JSON, merging @"schemaVersion"@ into the root object
-- alongside 'DiagnosticDTO''s own fields (never into a nested 'dtoRelated'
-- or 'CauseDTOFull' entry — only the true root carries it). An additive
-- change to the existing flat shape, not a new envelope wrapper: every key
-- a consumer already reads off the root is untouched.
renderJson :: Diagnostic e => JsonOptions -> e -> Value
renderJson opts e = case toJSON (toDTO cd depth (walkRelated depth root)) of
  Object o -> Object (KM.insert "schemaVersion" (toJSON currentSchemaVersion) o)
  v        -> v -- unreachable: DiagnosticDTO's ToJSON always produces an Object
  where
    root  = SomeDiagnostic e
    depth = joRelatedDepth opts
    cd    = joCauseDetail opts

-- | One entry of a diagnostic's cause chain. The chain is listed flat, in
-- order, on the diagnostic that owns it, so under 'CauseFull' an entry's own
-- @causes@ is always empty: its successors are the entries after it in the
-- owner's list, and nesting them again would repeat every later cause once per
-- earlier one. A full entry still carries its own labels, help and related
-- diagnostics. At zero @fuel@ an entry degrades to the summary shape.
causeDTO :: CauseDetail -> Natural -> Natural -> SomeDiagnostic -> CauseDTO
causeDTO CauseFull depth fuel c
  | fuel > 0 =
      CauseDTOFull ((toDTOFuel CauseFull depth (fuel - 1) (walkRelated depth c)) { dtoCauses = [] })
causeDTO _ _ _ (SomeDiagnostic e) =
  CauseDTOSummary (unDiagnosticCode <$> code e) (docToText (message e))

-- Plain-text rendering of an annotated document (annotations discarded); JSON
-- string values carry the raw message\/label\/help text.
docToText :: Doc Ann -> Text
docToText = T.map (\c -> if isControl c then ' ' else c)
          . renderStrict . layoutPretty (LayoutOptions Unbounded)
