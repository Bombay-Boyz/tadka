-- | Rendering configuration and the render 'Target' (vision §7). The 'Config'
-- constructor is not exported: values are built from 'defaultConfig' via the
-- @with*@ setters. 'selectRenderer' (in "Tadka.Internal.Render") is the only
-- reader of the internal field accessors, and the only place a @Config@ turns
-- into a renderer.
--
-- No compatibility guarantee.
module Tadka.Internal.Config
  ( -- * Target and modes
    Target (..)
  , ColorMode (..)
  , UnicodeMode (..)
    -- * Config (opaque)
  , Config
  , defaultConfig
  , defaultPalette
    -- * Setters
  , withColorMode
  , withUnicodeMode
  , withRelatedDepthLimit
  , withTabWidth
  , withLabelPalette
  , withTarget
    -- * Internal field accessors (for selectRenderer only; not re-exported by "Tadka")
  , configColorMode
  , configUnicodeMode
  , configRelatedDepth
  , configTabWidth
  , configPalette
  , configTarget
  ) where

import           Data.List.NonEmpty            (NonEmpty (..))
import           Numeric.Natural               (Natural)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)

import           Tadka.Internal.Related        (defaultRelatedDepth)

-- | The three render targets @tadka@ ships. Closed by design: a fourth,
-- custom target is permanently out of scope (vision §7).
data Target = TGraphical | TNarratable | TJson
  deriving (Eq, Show, Enum, Bounded)

-- | When to colourise graphical output.
data ColorMode = ColorAuto | ColorAlways | ColorNever
  deriving (Eq, Show, Enum, Bounded)

-- | When to use non-ASCII box-drawing/underline glyphs.
data UnicodeMode = UnicodeAuto | UnicodeAlways | UnicodeAscii
  deriving (Eq, Show, Enum, Bounded)

-- | Opaque rendering configuration. Build from 'defaultConfig' with the
-- @with*@ setters.
data Config = Config
  { configColorMode    :: ColorMode
  , configUnicodeMode  :: UnicodeMode
  , configRelatedDepth :: Natural
  , configPalette      :: NonEmpty AnsiStyle
  , configTabWidth     :: Int            -- ^ tab stop width for source rendering (>= 1)
  , configTarget       :: Maybe Target   -- ^ 'Nothing' = auto; 'Just' = explicit override.
  }
  deriving (Eq, Show)

-- | The default six-colour underline palette (vision §7), distinguishable
-- under light and dark themes; degrades to distinct underline characters under
-- 'ColorNever' (handled by the graphical handler in Phase 5).
defaultPalette :: NonEmpty AnsiStyle
defaultPalette =
  color Red :| [color Green, color Yellow, color Blue, color Magenta, color Cyan]

-- | Sensible defaults: auto colour, auto Unicode, depth limit 8, the default
-- palette, and no explicit target (auto).
defaultConfig :: Config
defaultConfig = Config
  { configColorMode    = ColorAuto
  , configUnicodeMode  = UnicodeAuto
  , configRelatedDepth = defaultRelatedDepth
  , configPalette      = defaultPalette
  , configTabWidth     = 4
  , configTarget       = Nothing
  }

withColorMode :: ColorMode -> Config -> Config
withColorMode m c = c { configColorMode = m }

withUnicodeMode :: UnicodeMode -> Config -> Config
withUnicodeMode m c = c { configUnicodeMode = m }

withRelatedDepthLimit :: Natural -> Config -> Config
withRelatedDepthLimit n c = c { configRelatedDepth = n }

-- | Tab stop width used when rendering source lines (tabs expand to the next
-- multiple of this width). Values below 1 are treated as 1 by the renderer.
withTabWidth :: Int -> Config -> Config
withTabWidth n c = c { configTabWidth = n }

withLabelPalette :: NonEmpty AnsiStyle -> Config -> Config
withLabelPalette p c = c { configPalette = p }

-- | Explicitly force a render target, overriding auto-detection.
withTarget :: Target -> Config -> Config
withTarget t c = c { configTarget = Just t }
