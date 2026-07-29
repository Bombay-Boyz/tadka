{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Graphical report handler (vision §7, spec Phase 5): header, gutter/snippet
-- layout with Unicode-width-correct carets, per-label underline cycling, stale
-- labels, help/see lines, and related-chain nesting via the shared Phase 3 walk.
--
-- The layout is built as explicit 'Text' lines (for exact column control) then
-- wrapped in 'pretty'. Under 'ColorNever' each label's underline uses a
-- distinct cycling character (@^@ \/ @~@ \/ @-@); under a colour mode the
-- character is @^@ and the palette (see 'labelStyle') distinguishes labels via
-- ANSI at the terminal boundary. Column maths go through
-- "Tadka.Internal.Width", so combining marks, East-Asian-width, and emoji are
-- accounted for. The @= see:@ line's URL is wrapped in an OSC 8 hyperlink
-- escape when 'HyperlinkMode' allows it (see 'hyperlink'), independently of
-- colour.
--
-- No compatibility guarantee.
module Tadka.Internal.Renderer.Graphical
  ( GraphicalOptions (..)
  , renderGraphical
    -- * Layout internals (exposed for property tests)
  , caretLayout
  , labelStyle
  , caretGlyph
  ) where

import           Data.Char                     (isControl)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NE
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Numeric.Natural               (Natural)
import           Prettyprinter                 (Doc, LayoutOptions (..),
                                                PageWidth (Unbounded), annotate,
                                                layoutPretty, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color)
import qualified Prettyprinter.Render.Terminal as Term
import           Prettyprinter.Render.Text     (renderStrict)

import           Tadka.Internal.Ann            (Ann)
import           Tadka.Internal.Config         (ColorMode (..), HyperlinkMode (..),
                                                UnicodeMode (..))
import           Tadka.Internal.Context        (Context (..), LabelKind (..),
                                                LabelState (..), Labeled (..))
import           Tadka.Internal.SourceCode     (SourceCode (..))
import           Tadka.Internal.Renderer.LinePlan (PlanEntry (..), planLines)
import           Tadka.Internal.Renderer.Layout   (CellKind (..), assignLanes,
                                                   cellAt, laneCount)
import           Tadka.Internal.Diagnostic     (Diagnostic (..), SomeDiagnostic (..))
import           Tadka.Internal.Related        (RelatedTree (..), TerminationReason (..),
                                                walkCauses, walkRelated)
import           Tadka.Internal.Span           (LineCol (..), ResolvedSpan, resolvedEnd,
                                                resolvedStart, spanLength)
import           Tadka.Internal.Types          (DiagnosticCode,
                                                Severity (..), SeverityLabels (..),
                                                Url, severityLabels,
                                                unDiagnosticCode, unLength, unUrl)
import           Tadka.Internal.Width          (displayColumnAt, expandTabs)

-- | Resolved settings the graphical handler renders from (populated only by
-- @selectRenderer@).
data GraphicalOptions = GraphicalOptions
  { goColorMode     :: ColorMode
  , goUnicodeMode   :: UnicodeMode
  , goHyperlinkMode :: HyperlinkMode
  , goPalette       :: NonEmpty AnsiStyle
  , goRelatedDepth  :: Natural
  , goTabWidth      :: Int
  , goContextLines  :: Maybe Int
  }
  deriving (Eq, Show)

-- | Box-drawing glyph set, selected by 'UnicodeMode'.
data Glyphs = Glyphs
  { gRail   :: Text   -- ^ vertical rail
  , gCorner :: Text   -- ^ top-left corner of the location line
  , gDash   :: Text   -- ^ horizontal dash following the corner
  , gEmDash :: Text   -- ^ em-dash used in prose separators
  , gVellip :: Text   -- ^ vertical ellipsis marking elided lines
  , gMLopen  :: Char  -- ^ multi-line span opening corner
  , gMLthru  :: Char  -- ^ multi-line span continuation
  , gMLclose :: Char  -- ^ multi-line span closing corner
  }

glyphsFor :: UnicodeMode -> Glyphs
glyphsFor UnicodeAscii = Glyphs "|" "+" "-" "-" "..." '/' '|' '\\'
glyphsFor _            = Glyphs "\x2502" "\x250C" "\x2500" "\x2014" "\x22EE" '\x256D' '\x2502' '\x2570'  -- │ ┌ ─ — ⋮ ╭ │ ╰

-- | Wrap text in the ANSI escapes for a style, unless colour is off. Uses the
-- terminal renderer so the escapes are correct; under 'ColorNever' the text is
-- returned untouched (so plain output — all golden fixtures — carries no ANSI).
colorize :: ColorMode -> AnsiStyle -> Text -> Text
colorize ColorNever _ t = t
colorize _ style t =
  Term.renderStrict (layoutPretty (LayoutOptions Unbounded) (annotate style (pretty t)))

-- | Wrap already-displayed text in an OSC 8 terminal-hyperlink escape pointing
-- at @url@, unless hyperlinks are off. Mirrors 'colorize': 'HyperlinkNever'
-- returns the text untouched (so plain output — every existing golden fixture
-- — carries no escape), any other mode wraps it.
--
-- Takes a 'Url', never raw 'Text', so only an already-validated value — one
-- that has passed 'Tadka.Internal.Types.mkUrl''s absolute-URI check — can ever
-- be interpolated into the escape. 'mkUrl' delegates to
-- 'Network.URI.parseAbsoluteURI', whose RFC 3986 grammar has no production
-- admitting a raw control character, so a 'Url''s text can never itself
-- contain the ESC byte that starts (or forges) a terminal escape sequence:
-- this wrap is injection-safe without any extra stripping here, by
-- construction rather than by runtime check.
hyperlink :: HyperlinkMode -> Url -> Text -> Text
hyperlink HyperlinkNever _ label  = label
hyperlink _              u label  = oscLinkStart u <> label <> oscLinkEnd

-- | The OSC 8 "open link" escape for a URL: empty params, the URL, then the
-- string terminator (see 'oscStringTerminator').
oscLinkStart :: Url -> Text
oscLinkStart u = "\ESC]8;;" <> unUrl u <> oscStringTerminator

-- | The OSC 8 "close link" escape: the same shape with an empty URL, the
-- terminal-side convention every OSC 8 implementation shares for ending the
-- link that the most recent 'oscLinkStart' opened.
oscLinkEnd :: Text
oscLinkEnd = "\ESC]8;;" <> oscStringTerminator

-- | The string terminator (@ST@, @ESC \\@) that ends an OSC escape sequence.
-- Preferred over the historical BEL (@\\a@) terminator: it is the form every
-- OSC-8-supporting terminal in current use (iTerm2, kitty, VTE-based
-- terminals, Windows Terminal, …) already documents and accepts.
oscStringTerminator :: Text
oscStringTerminator = "\ESC\\"

-- | Header colour by severity (bold + a conventional hue).
severityStyle :: Severity -> AnsiStyle
severityStyle SevError   = color Red    <> bold
severityStyle SevWarning = color Yellow <> bold
severityStyle SevAdvice  = color Blue   <> bold

-- | The underline character for label index @i@. Under 'ColorNever' the three
-- characters cycle so labels stay distinguishable in plain text; otherwise a
-- single @^@ is used (colour distinguishes labels at the terminal).
caretGlyph :: LabelKind -> Char
caretGlyph Primary   = '^'
caretGlyph Secondary = '-'

-- | The ANSI style for a label: primary labels take the (bold) severity colour;
-- secondary labels cycle the configured palette by index.
labelDisplayStyle :: Severity -> NonEmpty AnsiStyle -> LabelKind -> Int -> AnsiStyle
labelDisplayStyle sev _       Primary   _ = severityStyle sev
labelDisplayStyle _   palette Secondary i = labelStyle palette i

-- | The palette entry for label index @i@ (vision §7): entry @i `mod` p@ where
-- @p@ is the palette length. The single place this cycling formula lives.
labelStyle :: NonEmpty AnsiStyle -> Int -> AnsiStyle
labelStyle palette i =
  case drop (i `mod` NE.length palette) (NE.toList palette) of
    (s : _) -> s
    []      -> NE.head palette   -- unreachable: 0 <= i `mod` n < n (NE.head is total)

-- | Given a source line, a 1-based start column, and a character length,
-- compute @(displayColumnsBefore, caretWidth)@ using display widths. Both are
-- non-negative and @caretWidth >= 1@, so a caret can never sit at a negative
-- offset nor collapse to nothing.
-- | Given a tab width, a source line, a 1-based start column, and a character
-- length, compute @(displayColumnsBefore, caretWidth)@ using tab-aware display
-- columns. Both are non-negative and @caretWidth >= 1@; @displayColumnsBefore@
-- equals the display width of the tab-expanded source preceding the span, so
-- the caret can never sit at a negative offset, collapse to nothing, nor drift
-- out of alignment with a tab-indented source line.
caretLayout :: Int -> Text -> Int -> Int -> (Int, Int)
caretLayout tw srcLine startCol len = (dispStart, caretWidth)
  where
    dispStart  = displayColumnAt tw srcLine (max 0 (startCol - 1))
    dispEnd    = displayColumnAt tw srcLine (max 0 (startCol - 1) + max 0 len)
    caretWidth = max 1 (dispEnd - dispStart)

-- | Render a diagnostic to a graphical report 'Doc'.
renderGraphical :: Diagnostic e => GraphicalOptions -> e -> Doc Ann
renderGraphical opts e =
  pretty (T.intercalate "\n" (renderRoot opts (SomeDiagnostic e)))

-- Whole-report line list for the top-level diagnostic.
renderRoot :: GraphicalOptions -> SomeDiagnostic -> [Text]
renderRoot opts sd@(SomeDiagnostic e) =
     header : snip ++ sep ++ trailer
  where
    glyphs   = glyphsFor (goUnicodeMode opts)
    gw       = gutterWidth (context e)
    eqIndent = T.replicate (gw + 1) " "
    header   = headerLine (goColorMode opts) (severity e) (code e) (docToText (message e))
    snip     = snippetLines glyphs (severity e) (goColorMode opts) (goPalette opts) (goTabWidth opts) (goContextLines opts) gw (context e)
    trailer  = helpSeeLines gw (fmap docToText (help e))
                 (fmap (\u -> hyperlink (goHyperlinkMode opts) u (unUrl u)) (url e))
                 ++ causeLines
                 ++ relatedForest opts glyphs gw eqIndent (walkRelated (goRelatedDepth opts) sd)
    causeLines = [ eqIndent <> "= caused by: " <> summaryOf glyphs c
                 | c <- walkCauses (goRelatedDepth opts) sd ]
    sep      = [T.replicate (gw + 1) " " <> gRail glyphs | not (null snip) && not (null trailer)]

-- === Header ===============================================================

headerLine :: ColorMode -> Severity -> Maybe DiagnosticCode -> Text -> Text
headerLine cmode sev mcode msg =
  colorize cmode (severityStyle sev) (word <> codePart <> ":") <> " " <> msg
  where
    word     = T.takeWhile (/= ':') (severityGraphicalHeader (severityLabels sev))
    codePart = maybe "" (\c -> "[" <> unDiagnosticCode c <> "]") mcode

-- === Snippet ==============================================================

-- Width of the line-number gutter: enough for the largest displayed line
-- number, or 1 when there are no resolved labels.
gutterWidth :: Context -> Int
gutterWidth ctx = length (show (foldr max 1 (okEndLines ctx)))
  where
    okEndLines NoContext          = []
    okEndLines (HasLabels _ lbls) =
      [ lcLine (resolvedEnd rs) | Labeled (LabelOk rs) _ _ <- NE.toList lbls ]

snippetLines :: Glyphs -> Severity -> ColorMode -> NonEmpty AnsiStyle -> Int -> Maybe Int -> Int -> Context -> [Text]
snippetLines _ _ _ _ _ _ _ NoContext = []
snippetLines glyphs sev cmode palette tabW ctxLines gw (HasLabels src labels) =
  [locationLine, railBlank] ++ body
  where
    indexed = zip [0 ..] (NE.toList labels)
    oks   = [ (i, rs, k, docText txt) | (i, Labeled (LabelOk rs)   k txt) <- indexed ]
    stale = [ ()                      |     Labeled (LabelStale _) _ _   <- map snd indexed ]

    railIndent = T.replicate (gw + 1) " "
    railBlank  = railIndent <> gRail glyphs
    locationLine = railIndent <> gCorner glyphs <> gDash glyphs <> " " <> locText
    locText = maybe (scName src) posText locRs
    posText rs = scName src
                   <> ":" <> tshow (lcLine (resolvedStart rs))
                   <> ":" <> tshow (lcColumn (resolvedStart rs))
    locRs = case [ rs | (_, rs, Primary, _) <- oks ] of
      (rs:_) -> Just rs
      []     -> case oks of ((_, rs, _, _):_) -> Just rs; _ -> Nothing

    -- Multi-line spans get connector lanes; single-line labels get carets.
    startLineOf rs = lcLine (resolvedStart rs)
    endLineOf   rs = lcLine (resolvedEnd rs)
    isMulti     rs = startLineOf rs < endLineOf rs
    multis = [ (startLineOf rs, endLineOf rs, k, txt) | (_, rs, k, txt) <- oks, isMulti rs ]
    laneAssign = assignLanes [ (s, e) | (s, e, _, _) <- multis ]
    nLanes = laneCount laneAssign
    laneInfo = zipWith (\(lane, iv) (_, _, k, txt) -> (lane, iv, k, txt)) laneAssign multis
    gsep = if nLanes > 0 then T.singleton ' ' else T.empty
    laneIvAt lane l = case [ iv | (lane', iv@(s, e), _, _) <- laneInfo
                                , lane' == lane, s <= l, l <= e ] of
      (iv : _) -> Just iv
      []       -> Nothing
    glyphOfCell Open    = gMLopen glyphs
    glyphOfCell Through = gMLthru glyphs
    glyphOfCell Close   = gMLclose glyphs
    glyphOfCell Blank   = ' '
    srcGutter l = if nLanes == 0 then T.empty
                  else T.pack [ maybe ' ' (\iv -> glyphOfCell (cellAt iv l)) (laneIvAt lane l)
                              | lane <- [0 .. nLanes - 1] ] <> gsep
    contBelow lane l = case laneIvAt lane l of
      Just (s, e) -> s <= l && l < e
      Nothing     -> False
    caretGutter l = if nLanes == 0 then T.empty
                    else T.pack [ if contBelow lane l then gMLthru glyphs else ' '
                                | lane <- [0 .. nLanes - 1] ] <> gsep
    mlSuffix l = T.concat
      [ "  " <> colorize cmode (labelDisplayStyle sev palette k lane) t
      | (lane, (_, e), k, mt) <- laneInfo, e == l, Just t <- [mt] ]

    body = okBlock ++ staleBlock
    okBlock = case ([ startLineOf rs | (_, rs, _, _) <- oks ]
                    ++ [ endLineOf rs | (_, rs, _, _) <- oks, isMulti rs ]) of
      []       -> []
      (s : ss) -> concatMap (renderEntry win) plan
        where plan   = planLines ctxLines (scLineCount src) (s : ss)
              showns = [ l | ShowLine l <- plan ]
              win    = case showns of
                         (w : ws) -> scLines src (foldr min w ws, foldr max w ws)
                         []       -> []
    renderEntry _   (ElideLines _) = [railIndent <> gVellip glyphs]
    renderEntry win (ShowLine l)   = lineWithCarets win l
    lineWithCarets win l = numbered : caretsHere
      where
        srcLine  = sanitizeLine (lineLookup win l)
        numbered = T.justifyRight gw ' ' (tshow l) <> " " <> gRail glyphs <> " "
                     <> srcGutter l <> expandTabs tabW srcLine <> mlSuffix l
        caretsHere =
          [ caretLine l i rs k txt srcLine
          | (i, rs, k, txt) <- oks, not (isMulti rs), startLineOf rs == l ]
    caretLine l i rs k txt srcLine =
      railIndent <> gRail glyphs <> " " <> caretGutter l
        <> T.replicate dispStart " "
        <> colorize cmode (labelDisplayStyle sev palette k i)
             (T.replicate cw (T.singleton (caretGlyph k)) <> maybe "" (" " <>) txt)
      where
        (dispStart, cw) = caretLayout tabW srcLine (lcColumn (resolvedStart rs))
                                      (spanCharLen rs)

    staleBlock =
      [ railIndent <> gRail glyphs <> " (span unavailable "
          <> gEmDash glyphs <> " source no longer matches at this position)"
      | _ <- stale ]

spanCharLen :: ResolvedSpan -> Int
spanCharLen = unLength . spanLength

-- === Help / See ===========================================================

helpSeeLines :: Int -> Maybe Text -> Maybe Text -> [Text]
helpSeeLines gw mhelp murl =
     [ eqIndent <> "= help: " <> h | Just h <- [mhelp] ]
  ++ [ eqIndent <> "= see: "  <> u | Just u <- [murl] ]
  where eqIndent = T.replicate (gw + 1) " "

-- === Related nesting ======================================================

-- Render the related section under a parent, given the parent's eq-line indent.
relatedForest :: GraphicalOptions -> Glyphs -> Int -> Text -> RelatedTree -> [Text]
relatedForest opts glyphs _gw indent (RelatedTree parentDiag children term) =
     concatMap (relatedChild opts glyphs indent) children
  ++ depthNote
  where
    depthNote = case term of
      DepthTruncated ->
        [indent <> "= related: (" <> tshow (numRelated parentDiag)
                <> " more related diagnostics omitted)"]
      _ -> []

relatedChild :: GraphicalOptions -> Glyphs -> Text -> RelatedTree -> [Text]
relatedChild opts glyphs indent node@(RelatedTree childDiag _ term) =
  case term of
    CycleOmitted -> [indent <> "= related: (cycle omitted)"]
    _ ->
        (indent <> "= related: " <> summaryOf glyphs childDiag)
      : map (nest <>) (childSnippet ++ childSep ++ childRelated)
  where
    nest       = "  "
    childGw    = withDiag childDiag (gutterWidth . context)
    childSnippet = withDiag childDiag
      (\ce -> snippetLines glyphs (severity ce) (goColorMode opts) (goPalette opts) (goTabWidth opts) (goContextLines opts) childGw (context ce))
    childRelated = relatedForest opts glyphs childGw (T.replicate (childGw + 1) " ") node
    childSep     = [T.replicate (childGw + 1) " " <> gRail glyphs
                   | not (null childSnippet) && not (null childRelated)]

summaryOf :: Glyphs -> SomeDiagnostic -> Text
summaryOf glyphs (SomeDiagnostic e) =
  case code e of
    Just c  -> unDiagnosticCode c <> " " <> gEmDash glyphs <> " " <> msg
    Nothing -> msg
  where msg = docToText (message e)

numRelated :: SomeDiagnostic -> Int
numRelated (SomeDiagnostic e) = length (related e)

-- Apply a function that needs the concrete diagnostic behind a 'SomeDiagnostic'.
withDiag :: SomeDiagnostic -> (forall e. Diagnostic e => e -> a) -> a
withDiag (SomeDiagnostic e) f = f e

-- === Small helpers ========================================================

-- | Render a 'Doc' to a single line: embedded newlines (e.g. in a label or
-- message) are flattened to spaces so they can never break the caret/gutter
-- layout (mirrors miette's multi-line-label robustness fix, #318).
docToText :: Doc ann -> Text
docToText = T.map flat . renderStrict . layoutPretty (LayoutOptions Unbounded)
  where flat c = if isControl c then ' ' else c   -- strip control chars: no terminal-escape injection

docText :: Maybe (Doc Ann) -> Maybe Text
docText = fmap docToText

-- | Replace control characters (except tab, handled by 'expandTabs') with a
-- space, so attacker-controlled source can't inject terminal escape sequences.
-- Width-preserving (every control char is width 1), so caret columns are unmoved.
sanitizeLine :: Text -> Text
sanitizeLine = T.map (\c -> if isControl c && c /= '\t' then ' ' else c)

-- | Text of line @n@ within a fetched window, or @""@ if absent. Total.
lineLookup :: [(Int, Text)] -> Int -> Text
lineLookup win n = case [ t | (m, t) <- win, m == n ] of
  (t : _) -> t
  []      -> ""

tshow :: Show a => a -> Text
tshow = T.pack . show
