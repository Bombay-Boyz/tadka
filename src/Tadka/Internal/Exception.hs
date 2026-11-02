-- | The exception-propagation bridge "Tadka.Internal.Diagnostic" itself
-- deliberately omits.
--
-- README (and the original design): "Tadka does not replace your
-- application's error-propagation mechanism" — 'Diagnostic' has no
-- 'Control.Exception.Exception' superclass, no instance, and no
-- 'Show' requirement (its own 'message' method already renders it; forcing
-- 'Show' too would be a second, redundant rendering story). That is still
-- the right call for the common case, where an application's own
-- 'Either'\/'ExceptT'\/'MonadError' already carries the error.
--
-- But some code only propagates via exceptions — a caught
-- 'Control.Exception.SomeException' from a library, a top-level handler, a
-- boundary with code that isn't going to be rewritten to thread an
-- 'Either' through. For that code, a 'Diagnostic' value with no
-- 'Exception' instance is a dead end: it can't be 'throwIO''d or 'catch''d
-- at all without the caller hand-writing the bridge below for every single
-- error type. This module /is/ that bridge, written once: 'DiagnosticException'
-- wraps any 'Diagnostic' as an ordinary 'Exception', 'throwDiagnostic' and
-- 'catchDiagnostic' are the ordinary entry points, and 'installPanicHook'
-- (Phase, "the panic hook") makes an uncaught one print as a full report
-- instead of GHC's default @show@ dump.
--
-- No compatibility guarantee.
module Tadka.Internal.Exception
  ( DiagnosticException (..)
  , diagnosticShow
  , throwDiagnostic
  , catchDiagnostic
  , installPanicHook
  ) where

import           Control.Exception       (Exception (..), SomeException, catch, fromException,
                                          throwIO)
import qualified Data.Text               as T
import           System.IO               (stderr)
import           GHC.Conc                 (getUncaughtExceptionHandler,
                                           setUncaughtExceptionHandler)

import           Tadka.Internal.Config    (Config, defaultConfig)
import           Tadka.Internal.Diagnostic (Diagnostic, SomeDiagnostic (..))
import           Tadka.Internal.Render   (reportDiagnosticTo)
import           Tadka.Internal.Renderer.Narratable
                   (narratableOptions, renderNarratable)

-- | A 'Diagnostic' value, packaged as an ordinary GHC exception. The one,
-- documented way to make a 'Diagnostic' participate in
-- 'Control.Exception.throwIO'\/'Control.Exception.catch'-based propagation:
-- reach for 'throwDiagnostic'\/'catchDiagnostic' at ordinary call sites, and
-- for this constructor directly only when interop with code that pattern-
-- matches concrete exception types makes that unavoidable.
newtype DiagnosticException = DiagnosticException SomeDiagnostic

-- | Renders via the accessible (narratable) handler at its defaults — the
-- same prose a screen reader would get — since 'Show'\/'displayException'
-- must be pure and deterministic, with no terminal to detect capabilities
-- against. Reach for 'Tadka.Internal.Render.reportDiagnostic' (or
-- 'installPanicHook') instead when a full, capability-aware graphical
-- report is what's wanted.
instance Show DiagnosticException where
  show (DiagnosticException (SomeDiagnostic e)) = diagnosticShow e

instance Exception DiagnosticException

-- | The plain-prose rendering 'DiagnosticException''s 'Show' instance uses,
-- exposed directly for a caller's own 'Show'\/'Exception' instance to
-- delegate to — the idiomatic alternative to the generic wrapper above: give
-- a concrete error type its own 'Exception' instance (@instance Exception
-- MyError@, using the default @displayException = show@) with
-- @instance Show MyError where show = diagnosticShow@, and it becomes
-- 'throwIO'\/'catch'-able /as that exact type/, with GHC's ordinary
-- type-directed exception dispatch distinguishing it from any other
-- exception — no wrapper, no 'Data.Typeable' downcasting required.
diagnosticShow :: Diagnostic e => e -> String
diagnosticShow = T.unpack . renderNarratable (narratableOptions defaultConfig)

-- | Throw a 'Diagnostic' as an ordinary GHC exception, wrapped in
-- 'DiagnosticException'.
throwDiagnostic :: Diagnostic e => e -> IO a
throwDiagnostic = throwIO . DiagnosticException . SomeDiagnostic

-- | Catch any exception thrown via 'throwDiagnostic' (or a direct
-- 'Control.Exception.throwIO' of a 'DiagnosticException'), getting back the
-- 'SomeDiagnostic' it carried — not the wrapper, which a caller going
-- through this function never otherwise needs to know exists.
catchDiagnostic :: IO a -> (SomeDiagnostic -> IO a) -> IO a
catchDiagnostic action handler =
  action `catch` \(DiagnosticException sd) -> handler sd

-- | Install the given rendering configuration as GHC's top-level uncaught-
-- exception printer — the Haskell analogue of miette's @set_panic_hook@.
-- Once installed, an uncaught 'DiagnosticException' (however it was
-- thrown — 'throwDiagnostic' or a direct @throwIO@) prints as a full,
-- capability-aware diagnostic report, written to @stderr@ (colour and
-- Unicode are detected against @stderr@, not @stdout@), instead of GHC's
-- default @show@ dump. If rendering itself throws, the previously installed
-- handler reports the exception instead; any other uncaught exception still
-- falls back to whichever handler was previously installed (GHC's own
-- default, unless something else already called this), completely
-- unchanged.
--
-- Built on 'GHC.Conc.setUncaughtExceptionHandler' — the one documented IO
-- hook into GHC's "last gasp" exception printer, not a reimplementation of
-- it. That function has been part of @base@ since GHC 9.4 (@base-4.17@),
-- comfortably within this package's @base >= 4.18@ floor, but its exact
-- re-export path has moved before as GHC's @ghc-internal@ split evolves;
-- if a future GHC version stops re-exporting it from 'GHC.Conc', the fix is
-- a one-line import change here, not a design change.
installPanicHook :: Config -> IO ()
installPanicHook cfg = do
  fallback <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler (handler fallback)
  where
    handler :: (SomeException -> IO ()) -> SomeException -> IO ()
    handler fallback se = case fromException se of
      Just (DiagnosticException (SomeDiagnostic e)) ->
        reportDiagnosticTo stderr cfg e `catch` \(_ :: SomeException) -> fallback se
      Nothing -> fallback se
