
--
-- No compatibility guarantee.
module Tadka.Internal.Config
  ( -- * Target and modes
    Target (..)
  , ColorMode (..)
  , UnicodeMode (..)
  , HyperlinkMode (..)
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
    -- * Internal field accessors (for selectRenderer only; not re-exported by "Tadka")
  , configColorMode
  , configUnicodeMode
  , configHyperlinkMode
  , configRelatedDepth
  , configTabWidth
  , configContextLines
  , configPalette
  , configTarget
  ) where

import           Data.List.NonEmpty            (NonEmpty (..))
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
