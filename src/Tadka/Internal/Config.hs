
--
-- No compatibility guarantee.
module Tadka.Internal.Config
  ( -- * Target and modes
    Target (..)
  , ColorMode (..)
  , UnicodeMode (..)
  , HyperlinkMode (..)
  , CauseDetail (..)
  , Highlighter (..)
    -- * Config (opaque)
  , Config
  , defaultConfig
  , defaultPalette
    -- * Setters
  , withColorMode
  , withUnicodeMode
  , withHyperlinkMode
  , withRelatedDepthLimit
  , withTabWidth
  , withContextLines
  , withLabelPalette
  , withTarget
  , withCauseDetail
  , withHighlighter
  , withoutHighlighter
    -- * Internal field accessors (for selectRenderer only; not re-exported by "Tadka")
  , configColorMode
  , configUnicodeMode
  , configHyperlinkMode
  , configRelatedDepth
  , configTabWidth
  , configContextLines
  , configPalette
  , configTarget
  , configCauseDetail
  , configHighlighter
  ) where

import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Text                     (Text)
import           Numeric.Natural               (Natural)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)

import           Tadka.Internal.Related        (defaultRelatedDepth)

-- | The three render targets @tadka@ ships. Closed by design: a fourth,
-- custom target is permanently out of scope.
data Target = TGraphical | TNarratable | TJson
  deriving (Eq, Show, Enum, Bounded)

-- | When to colourise graphical output.
data ColorMode = ColorAuto | ColorAlways | ColorNever
  deriving (Eq, Show, Enum, Bounded)

-- | When to use non-ASCII box-drawing/underline glyphs.
data UnicodeMode = UnicodeAuto | UnicodeAlways | UnicodeAscii
  deriving (Eq, Show, Enum, Bounded)

-- | When to wrap displayed URLs (currently just the @= see:@ line, vision §7)
-- in an OSC 8 terminal hyperlink escape, so a supporting terminal renders them
-- as clickable text instead of plain text a user must select and open by hand.
data HyperlinkMode = HyperlinkAuto | HyperlinkAlways | HyperlinkNever
  deriving (Eq, Show, Enum, Bounded)

-- | How a diagnostic's @diagnosticCause@ chain is rendered, relative to its
-- (tree-shaped) @related@ diagnostics, which always get a fully nested
-- report. 'CauseSummary' is the historical, default behaviour (a single
-- @"= caused by: ..."@ line per cause, carrying only its code and message);
-- 'CauseFull' nests each cause's own snippet, help\/see lines, and related
-- forest, so a cause that is itself a rich 'Diagnostic' keeps its own spans
-- and labels in the report. It applies to the cause chain of the diagnostic
-- being reported; the causes of its related diagnostics are always shown as
-- summary lines, which bounds the size of the report.
data CauseDetail = CauseSummary | CauseFull
  deriving (Eq, Show, Enum, Bounded)

-- | A syntax highlighter for source snippets, supplied by the application
-- (or by the separate @tadka-highlight@ package). The core library has no
-- tokenizer of its own and no dependency on one.
--
-- 'runHighlighter' receives the source's name (usually a file path) and
-- /all/ of the source's lines, already tab-expanded and with control
-- characters replaced by spaces. It returns 'Nothing' when it declines to
-- highlight this source, or exactly one output line per input line. The
-- renderer only accepts an output line if removing its SGR colour escapes
-- (@ESC [ ... m@) gives back exactly the plain line, so a highlighter can add
-- colour but can never change the text, move a caret, or inject any other
-- terminal escape. A line failing that check is shown as plain text.
--
-- Highlighting is ignored under 'ColorNever'.
data Highlighter = Highlighter
  { highlighterName :: Text
    -- ^ identifies the highlighter; the only thing 'Eq' and 'Show' compare\/show.
  , runHighlighter  :: Text -> [Text] -> Maybe [Text]
    -- ^ source name -> plain lines -> highlighted lines.
  }

instance Eq Highlighter where
  a == b = highlighterName a == highlighterName b

instance Show Highlighter where
  showsPrec d h = showParen (d > 10) $
    showString "Highlighter " . shows (highlighterName h)

-- | Opaque rendering configuration. Build from 'defaultConfig' with the
-- @with*@ setters.
data Config = Config
  { configColorMode     :: ColorMode
  , configUnicodeMode   :: UnicodeMode
  , configHyperlinkMode :: HyperlinkMode
  , configRelatedDepth  :: Natural
  , configPalette       :: NonEmpty AnsiStyle
  , configTabWidth      :: Int            -- ^ tab stop width for source rendering (>= 1)
  , configContextLines  :: Maybe Int      -- ^ 'Nothing' = contiguous range; 'Just' n = n context lines + elision
  , configTarget        :: Maybe Target   -- ^ 'Nothing' = auto; 'Just' = explicit override.
  , configCauseDetail   :: CauseDetail    -- ^ summary line vs. full nested report per cause.
  , configHighlighter   :: Maybe Highlighter  -- ^ 'Nothing' = plain snippets (the default).
  }
  deriving (Eq, Show)

-- | The default six-colour underline palette, distinguishable
-- under light and dark themes; degrades to distinct underline characters under
-- 'ColorNever' (handled by the graphical handler in Phase 5).
defaultPalette :: NonEmpty AnsiStyle
defaultPalette =
  color Red :| [color Green, color Yellow, color Blue, color Magenta, color Cyan]

-- | Sensible defaults: auto colour, auto Unicode, hyperlinks off, depth limit
-- 8, the default palette, and no explicit target (auto).
--
-- Hyperlinks default to 'HyperlinkNever' rather than 'HyperlinkAuto', unlike
-- colour/Unicode: OSC 8 has no reliable capability query the way TTY-ness
-- does, so a terminal that reports 'capIsTerminal' may still not render the
-- escape as a link. Defaulting off keeps existing callers' output
-- byte-for-byte unchanged until they opt in with 'withHyperlinkMode'.
defaultConfig :: Config
defaultConfig = Config
  { configColorMode     = ColorAuto
  , configUnicodeMode   = UnicodeAuto
  , configHyperlinkMode = HyperlinkNever
  , configRelatedDepth  = defaultRelatedDepth
  , configPalette       = defaultPalette
  , configTabWidth      = 4
  , configContextLines  = Nothing
  , configTarget        = Nothing
  , configCauseDetail   = CauseSummary
  , configHighlighter   = Nothing
  }

withColorMode :: ColorMode -> Config -> Config
withColorMode m c = c { configColorMode = m }

withUnicodeMode :: UnicodeMode -> Config -> Config
withUnicodeMode m c = c { configUnicodeMode = m }

withHyperlinkMode :: HyperlinkMode -> Config -> Config
withHyperlinkMode m c = c { configHyperlinkMode = m }

withRelatedDepthLimit :: Natural -> Config -> Config
withRelatedDepthLimit n c = c { configRelatedDepth = n }

-- | Tab stop width used when rendering source lines (tabs expand to the next
-- multiple of this width). Values below 1 are treated as 1 by the renderer.
withTabWidth :: Int -> Config -> Config
withTabWidth n c = c { configTabWidth = n }


withContextLines :: Int -> Config -> Config
withContextLines n c = c { configContextLines = Just n }

withLabelPalette :: NonEmpty AnsiStyle -> Config -> Config
withLabelPalette p c = c { configPalette = p }

-- | Explicitly force a render target, overriding auto-detection.
withTarget :: Target -> Config -> Config
withTarget t c = c { configTarget = Just t }

-- | Choose how a diagnostic's cause chain is rendered: a summary line
-- ('CauseSummary', the default) or a full nested report per cause
-- ('CauseFull').
withCauseDetail :: CauseDetail -> Config -> Config
withCauseDetail d c = c { configCauseDetail = d }

-- | Syntax-highlight source snippets with the given 'Highlighter'. Off by
-- default.
withHighlighter :: Highlighter -> Config -> Config
withHighlighter h c = c { configHighlighter = Just h }

-- | Turn syntax highlighting off (the default).
withoutHighlighter :: Config -> Config
withoutHighlighter c = c { configHighlighter = Nothing }
