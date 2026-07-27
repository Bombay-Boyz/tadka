-- | Terminal-capability detection and the pure resolution of @Auto@ render
-- modes into concrete ones (post-v1 hardening). Detection (the only IO here)
-- gathers capabilities; 'resolveConfig' — pure and total — turns 'ColorAuto'
-- and 'UnicodeAuto' into concrete modes, so 'Tadka.Internal.Render.selectRenderer'
-- stays pure. Explicit modes are always passed through unchanged.
--
-- No compatibility guarantee.
module Tadka.Internal.Terminal
  ( TerminalCaps (..)
  , detectTerminalCaps
  , resolveColor
  , resolveUnicode
  , resolveConfig
  ) where

import           Data.Maybe          (isJust, listToMaybe)
import qualified Data.Text           as T
import           System.Environment  (lookupEnv)
import           System.IO           (Handle, hIsTerminalDevice)

import           Tadka.Internal.Config (ColorMode (..), Config, UnicodeMode (..),
                                        configColorMode, configUnicodeMode,
                                        withColorMode, withUnicodeMode)

-- | Detected capabilities of an output destination and environment.
data TerminalCaps = TerminalCaps
  { capIsTerminal :: Bool   -- ^ the handle is an interactive terminal
  , capNoColor    :: Bool   -- ^ @NO_COLOR@ is present (any value)
  , capForceColor :: Bool   -- ^ @CLICOLOR_FORCE@ is present and not @"0"@
  , capUnicode    :: Bool   -- ^ the active locale looks UTF-8
  }
  deriving (Eq, Show)

-- | Gather capabilities for a handle: TTY status, @NO_COLOR@\/@CLICOLOR_FORCE@,
-- and a UTF-8 locale check (@LC_ALL@ > @LC_CTYPE@ > @LANG@, POSIX precedence).
detectTerminalCaps :: Handle -> IO TerminalCaps
detectTerminalCaps h = do
  term    <- hIsTerminalDevice h
  noColor <- isJust <$> lookupEnv "NO_COLOR"
  force   <- maybe False (/= "0") <$> lookupEnv "CLICOLOR_FORCE"
  uni     <- localeIsUtf8
  pure TerminalCaps { capIsTerminal = term, capNoColor = noColor
                    , capForceColor = force, capUnicode = uni }

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

-- | Resolve both @Auto@ modes in a 'Config' to concrete modes.
resolveConfig :: TerminalCaps -> Config -> Config
resolveConfig caps cfg =
    withUnicodeMode (resolveUnicode caps (configUnicodeMode cfg))
  . withColorMode   (resolveColor   caps (configColorMode   cfg))
  $ cfg
