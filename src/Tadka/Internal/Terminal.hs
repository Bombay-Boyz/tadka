-- | Terminal-capability detection and the pure resolution of @Auto@ render
-- modes into concrete ones (post-v1 hardening). Detection (the only IO here)
-- gathers capabilities; 'resolveConfig' — pure and total — turns 'ColorAuto',
-- 'UnicodeAuto', and 'HyperlinkAuto' into concrete modes, so
-- 'Tadka.Internal.Render.selectRenderer' stays pure. Explicit modes are always
-- passed through unchanged.
--
-- No compatibility guarantee.
module Tadka.Internal.Terminal
  ( TerminalCaps (..)
  , detectTerminalCaps
  , handleIsUnicode
  , resolveColor
  , resolveUnicode
  , resolveHyperlink
  , resolveConfig
  ) where

import           Data.Maybe          (isJust, listToMaybe)
import qualified Data.Text           as T
import           System.Environment  (lookupEnv)
import           Data.Char           (toUpper)
import           System.IO           (Handle, hGetEncoding, hIsTerminalDevice)

import           Tadka.Internal.Config (ColorMode (..), Config, HyperlinkMode (..),
                                        UnicodeMode (..), configColorMode,
                                        configHyperlinkMode, configUnicodeMode,
                                        withColorMode, withHyperlinkMode,
                                        withUnicodeMode)

-- | Detected capabilities of an output destination and environment.
data TerminalCaps = TerminalCaps
  { capIsTerminal      :: Bool   -- ^ the handle is an interactive terminal
  , capNoColor         :: Bool   -- ^ @NO_COLOR@ is present (any value)
  , capForceColor      :: Bool   -- ^ @CLICOLOR_FORCE@ is present and not @"0"@
  , capUnicode         :: Bool   -- ^ the locale looks UTF-8 and the handle's own encoding is a Unicode one
  , capNoHyperlink     :: Bool   -- ^ @NO_HYPERLINK@ is present (any value)
  , capForceHyperlink  :: Bool   -- ^ @FORCE_HYPERLINK@ is present and not @"0"@
  }
  deriving (Eq, Show)

-- | Gather capabilities for a handle: TTY status, @NO_COLOR@\/@CLICOLOR_FORCE@,
-- a UTF-8 locale check (@LC_ALL@ > @LC_CTYPE@ > @LANG@, POSIX precedence), and
-- the hyperlink pair below.
--
-- @FORCE_HYPERLINK@ is the convention the @supports-hyperlinks@ package (used
-- by Yarn and other JS CLIs) already established for forcing OSC 8 on.
-- @NO_HYPERLINK@ has no equivalent cross-tool precedent the way @NO_COLOR@
-- does; it is a tadka-local variable that simply mirrors @NO_COLOR@'s shape
-- (any value disables) for consistency and so a future shared convention could
-- slot in without an API change.
detectTerminalCaps :: Handle -> IO TerminalCaps
detectTerminalCaps h = do
  term        <- hIsTerminalDevice h
  noColor     <- isJust <$> lookupEnv "NO_COLOR"
  force       <- maybe False (/= "0") <$> lookupEnv "CLICOLOR_FORCE"
  localeUtf8  <- localeIsUtf8
  handleUtf8  <- handleIsUnicode h
  let uni = localeUtf8 && handleUtf8
  noHyper     <- isJust <$> lookupEnv "NO_HYPERLINK"
  forceHyper  <- maybe False (/= "0") <$> lookupEnv "FORCE_HYPERLINK"
  pure TerminalCaps { capIsTerminal = term, capNoColor = noColor
                    , capForceColor = force, capUnicode = uni
                    , capNoHyperlink = noHyper, capForceHyperlink = forceHyper }

-- | Whether the handle's own text encoding is a Unicode one (UTF-8, UTF-16,
-- UTF-32). The locale environment variables say what the terminal probably
-- supports, but the encoding actually installed on the 'Handle' decides
-- whether writing a non-ASCII character succeeds, and the two can differ
-- (a C locale, an @hSetEncoding@ call, a binary-mode handle, which has no
-- text encoding at all and is treated as not Unicode).
handleIsUnicode :: Handle -> IO Bool
handleIsUnicode h = do
  menc <- hGetEncoding h
  pure $ case menc of
    Nothing  -> False
    Just enc -> take 3 (map toUpper (show enc)) == "UTF"

localeIsUtf8 :: IO Bool
localeIsUtf8 = do
  vals <- traverse lookupEnv ["LC_ALL", "LC_CTYPE", "LANG"]
  let active = listToMaybe [ v | Just v <- vals, not (null v) ]
  pure (maybe False (T.isInfixOf (T.pack "utf") . T.toLower . T.pack) active)

-- | Resolve a colour mode against capabilities. Explicit modes pass through;
-- @Auto@ obeys @NO_COLOR@ (off), then @CLICOLOR_FORCE@ (on), then TTY status.
resolveColor :: TerminalCaps -> ColorMode -> ColorMode
resolveColor _    ColorAlways = ColorAlways
resolveColor _    ColorNever  = ColorNever
resolveColor caps ColorAuto
  | capNoColor caps    = ColorNever
  | capForceColor caps = ColorAlways
  | capIsTerminal caps = ColorAlways
  | otherwise          = ColorNever

-- | Resolve a Unicode mode against capabilities. Explicit modes pass through;
-- @Auto@ becomes 'UnicodeAlways' on a UTF-8 locale, else 'UnicodeAscii'.
resolveUnicode :: TerminalCaps -> UnicodeMode -> UnicodeMode
resolveUnicode _    UnicodeAlways = UnicodeAlways
resolveUnicode _    UnicodeAscii  = UnicodeAscii
resolveUnicode caps UnicodeAuto
  | capUnicode caps = UnicodeAlways
  | otherwise       = UnicodeAscii

-- | Resolve a hyperlink mode against capabilities. Explicit modes pass
-- through; @Auto@ obeys @NO_HYPERLINK@ (off), then @FORCE_HYPERLINK@ (on),
-- then TTY status — the same three-tier shape as 'resolveColor', since both
-- ultimately answer "can this destination usefully show a terminal escape?".
resolveHyperlink :: TerminalCaps -> HyperlinkMode -> HyperlinkMode
resolveHyperlink _    HyperlinkAlways = HyperlinkAlways
resolveHyperlink _    HyperlinkNever  = HyperlinkNever
resolveHyperlink caps HyperlinkAuto
  | capNoHyperlink caps    = HyperlinkNever
  | capForceHyperlink caps = HyperlinkAlways
  | capIsTerminal caps     = HyperlinkAlways
  | otherwise              = HyperlinkNever

-- | Resolve all three @Auto@ modes in a 'Config' to concrete modes.
resolveConfig :: TerminalCaps -> Config -> Config
resolveConfig caps cfg =
    withUnicodeMode   (resolveUnicode   caps (configUnicodeMode   cfg))
  . withColorMode     (resolveColor     caps (configColorMode     cfg))
  . withHyperlinkMode (resolveHyperlink caps (configHyperlinkMode cfg))
  $ cfg
