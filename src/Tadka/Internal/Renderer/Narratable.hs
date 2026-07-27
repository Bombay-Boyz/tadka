{-# LANGUAGE OverloadedStrings #-}

-- | Narratable (accessibility-first) report handler (vision §7, spec Phase 6):
-- prose, not layout. Every field the graphical handler can show has a prose
-- equivalent here — nothing is silently dropped between renderers.
--
-- 'Ann' is interpreted for prose via 'toProseMarker' (e.g. inline code is
-- surrounded with quotes), so annotated message\/label\/help content reads
-- naturally aloud.
--
-- No compatibility guarantee.
module Tadka.Internal.Renderer.Narratable
  ( NarratableOptions (..)
  , renderNarratable
  , toProseMarker
  ) where

import qualified Data.List.NonEmpty                    as NE
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Numeric.Natural                       (Natural)
import           Prettyprinter                         (Doc, LayoutOptions (..),
                                                        PageWidth (Unbounded),
                                                        layoutPretty)
import           Prettyprinter.Render.Util.SimpleDocTree (renderSimplyDecorated, treeForm)

import           Tadka.Internal.Ann                    (Ann (..))
import           Tadka.Internal.Context                (Context (..), LabelKind (..),
                                                        LabelState (..), Labeled (..))
import           Tadka.Internal.Diagnostic             (Diagnostic (..), SomeDiagnostic (..))
import           Tadka.Internal.Related                (RelatedTree (..),
                                                        TerminationReason (..), walkCauses,
                                                        walkRelated)
import           Tadka.Internal.Span                   (LineCol (..),
                                                        StaleReason (..), resolvedStart,
                                                        spanLength)
import           Tadka.Internal.Types                  (Severity, SeverityLabels (..),
                                                        severityLabels, sourceName,
                                                        sourceText, unDiagnosticCode,
                                                        unLength, unUrl)

-- | Resolved settings for the narratable handler (populated only by
-- @selectRenderer@).
newtype NarratableOptions = NarratableOptions
  { noRelatedDepth :: Natural }
  deriving (Eq, Show)

-- | How an 'Ann' is marked in prose. Inline code and file names are surrounded
-- with quotes so they read as distinct tokens; emphasis and keywords carry no
-- prose marker.
toProseMarker :: Ann -> Text
toProseMarker AnnCode     = "\""
toProseMarker AnnFilename = "\""
toProseMarker AnnEmphasis = ""
toProseMarker AnnKeyword  = ""

-- | Render an annotated document to prose, wrapping each annotated span with
-- its 'toProseMarker'.
docToProse :: Doc Ann -> Text
docToProse =
    renderSimplyDecorated id wrap . treeForm . layoutPretty (LayoutOptions Unbounded)
  where wrap ann inner = toProseMarker ann <> inner <> toProseMarker ann

renderNarratable :: Diagnostic e => NarratableOptions -> e -> Text
renderNarratable opts e = T.intercalate "\n" (renderProse opts (SomeDiagnostic e))

renderProse :: NarratableOptions -> SomeDiagnostic -> [Text]
renderProse opts sd@(SomeDiagnostic e) =
     headerSentence (severity e) (fmap unDiagnosticCode (code e)) (docToProse (message e))
   : contextSentences (context e)
  ++ helpSentences (fmap docToProse (help e)) (fmap unUrl (url e))
  ++ causeSentences (walkCauses (noRelatedDepth opts) sd)
  ++ relatedSentences (walkRelated (noRelatedDepth opts) sd)

-- === Header ===============================================================

headerSentence :: Severity -> Maybe Text -> Text -> Text
headerSentence sev mcode msg = prefix <> " " <> codeClause <> msg <> "."
  where
    prefix     = severityNarratablePrefix (severityLabels sev)     -- e.g. "Error,"
    codeClause = maybe "" (\c -> "code " <> c <> ": ") mcode

-- === Context (location, source, labels) ===================================

contextSentences :: Context -> [Text]
contextSentences NoContext = []
contextSentences (HasLabels src labels) = locationSentence ++ labelReadouts
  where
    indexed = NE.toList labels
    srcls   = T.splitOn "\n" (sourceText src)
    oks     = [ (k, rs) | Labeled (LabelOk rs) k _ <- indexed ]

    locRs = case [ rs | (Primary, rs) <- oks ] of
      (rs:_) -> Just rs
      []     -> case oks of ((_, rs):_) -> Just rs; _ -> Nothing
    locationSentence = case locRs of
      Just rs -> [ "Location: " <> sourceName src
                     <> ", line "   <> tshow (lcLine (resolvedStart rs))
                     <> ", column " <> tshow (lcColumn (resolvedStart rs)) <> "." ]
      Nothing -> [ "Location: " <> sourceName src <> "." ]

    labelReadouts = concatMap readout indexed
    readout (Labeled (LabelOk rs) k txt) =
      [ "Source line " <> tshow n <> ": \"" <> lineAt srcls n <> "\"."
      , leadIn k <> colClause <> labeled ]
      where
        n       = lcLine (resolvedStart rs)
        startC  = lcColumn (resolvedStart rs)
        effLen  = max 1 (unLength (spanLength rs))
        endC    = startC + effLen - 1
        colClause | startC == endC = "column " <> tshow startC
                  | otherwise      = "columns " <> tshow startC <> " through " <> tshow endC
        labeled = maybe "." (\t -> ", labeled: " <> docToProse t <> ".") txt
    readout (Labeled (LabelStale reason) _ txt) = [ staleSentence reason txt ]

    leadIn Primary   = "The problem is at "
    leadIn Secondary = "Related context is at "

staleSentence :: StaleReason -> Maybe (Doc Ann) -> Text
staleSentence reason txt =
  "A labeled position could not be shown because " <> reasonText reason
    <> maybe "." (\t -> ", labeled: " <> docToProse t <> ".") txt
  where
    reasonText SpanOutOfBounds = "the span is out of bounds for the current source"
    reasonText SourceMismatch  = "the source no longer matches at that position"

-- === Help / URL ===========================================================

helpSentences :: Maybe Text -> Maybe Text -> [Text]
helpSentences mhelp murl =
     [ "Help: " <> h             | Just h <- [mhelp] ]
  ++ [ "More information: " <> u | Just u <- [murl] ]

-- === Related ==============================================================

relatedSentences :: RelatedTree -> [Text]
relatedSentences (RelatedTree rootDiag children term) =
     concatMap relatedChild children
  ++ truncationNote term (numRelated rootDiag)

relatedChild :: RelatedTree -> [Text]
relatedChild (RelatedTree childDiag kids term) = case term of
  CycleOmitted -> [ "A related diagnostic was omitted because it forms a cycle." ]
  _ ->
       ("Related: " <> summaryProse childDiag <> ".")
     : concatMap relatedChild kids
    ++ truncationNote term (numRelated childDiag)

truncationNote :: TerminationReason -> Int -> [Text]
truncationNote DepthTruncated n =
  [ tshow n <> " more related diagnostic" <> plural n <> " omitted at the depth limit." ]
truncationNote _ _ = []

plural :: Int -> Text
plural 1 = " was"
plural _ = "s were"

causeSentences :: [SomeDiagnostic] -> [Text]
causeSentences = map (\c -> "Caused by: " <> summaryProse c <> ".")

summaryProse :: SomeDiagnostic -> Text
summaryProse (SomeDiagnostic e) = case code e of
  Just c  -> unDiagnosticCode c <> " \x2014 " <> msg    -- em dash
  Nothing -> msg
  where msg = docToProse (message e)

numRelated :: SomeDiagnostic -> Int
numRelated (SomeDiagnostic e) = length (related e)

-- === Helpers ==============================================================

lineAt :: [Text] -> Int -> Text
lineAt ls n
  | n >= 1 && n <= length ls = ls !! (n - 1)
  | otherwise                = ""

tshow :: Show a => a -> Text
tshow = T.pack . show
