{-# LANGUAGE OverloadedStrings #-}

-- | The "I just want /something/ printed nicely right now" escape hatch —
-- miette's blanket @IntoDiagnostic@ for any @std::error::Error@, its
-- @miette!("...")@ ad hoc constructor, @wrap_err@, and @with_source_code()@,
-- in one minimal 'Diagnostic' instance with nothing but a message.
--
-- Every other error type that wants a diagnostic still writes (or derives,
-- via "Tadka.Internal.TH") a full 'Diagnostic' instance up front — that
-- discipline is unchanged and is the right default. 'AdHocDiagnostic' is for
-- the other case: an arbitrary 'Show'-only error, or a caught
-- 'Control.Exception.SomeException', that needs to become /something/
-- diagnostic-shaped right now, without writing a dedicated type for it; and
-- for attaching source to an error after the fact, when the place that
-- raised it had none in scope.
--
-- No compatibility guarantee.
module Tadka.Internal.AdHoc
  ( AdHocDiagnostic
  , adHoc
  , wrapErr
  , intoDiagnostic
  , fromError
  , fromSomeException
  , withCode
  , withSeverity
  , withHelp
  , withUrl
  , withSourceCode
  ) where

import           Control.Exception        (Exception, SomeException, displayException)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Prettyprinter             (Doc, pretty)

import           Tadka.Internal.Ann        (Ann)
import           Tadka.Internal.Context    (Context (NoContext), buildContext)
import           Tadka.Internal.Diagnostic (Diagnostic (..), SomeDiagnostic (..))
import           Tadka.Internal.SourceCode (SourceCode)
import           Tadka.Internal.Span       (Span)
import           Tadka.Internal.Types
                   (DiagnosticCode, Severity (SevError), Url)

-- | A diagnostic with nothing but a message, and optionally a cause
-- ('wrapErr') or source ('withSourceCode') attached after the fact. The
-- constructor and its fields are not exported — opaque, like every other
-- smart-constructed type in this library — so build one with 'adHoc',
-- 'wrapErr', 'intoDiagnostic', or 'fromSomeException'.
data AdHocDiagnostic = AdHocDiagnostic
  { adHocMessage  :: Doc Ann
  , adHocContext  :: Context
  , adHocCode     :: Maybe DiagnosticCode
  , adHocSeverity :: Severity
  , adHocHelp     :: Maybe (Doc Ann)
  , adHocUrl      :: Maybe Url
  , adHocCause    :: Maybe SomeDiagnostic
  }

instance Diagnostic AdHocDiagnostic where
  message         = adHocMessage
  context         = adHocContext
  code            = adHocCode
  severity        = adHocSeverity
  help            = adHocHelp
  url             = adHocUrl
  diagnosticCause = adHocCause

-- | Build a minimal diagnostic from plain text: no source, no labels, no
-- cause. The direct analogue of miette's @miette!("...")@ macro and
-- @Report::msg@.
adHoc :: Text -> AdHocDiagnostic
adHoc msg = AdHocDiagnostic
  { adHocMessage  = pretty msg
  , adHocContext  = NoContext
  , adHocCode     = Nothing
  , adHocSeverity = SevError
  , adHocHelp     = Nothing
  , adHocUrl      = Nothing
  , adHocCause    = Nothing
  }

-- | 'adHoc', with an existing diagnostic attached as this one's cause — the
-- @wrap_err@ pattern: add ad hoc context /above/ an error without writing a
-- dedicated type for the wrapper.
wrapErr :: Diagnostic e => Text -> e -> AdHocDiagnostic
wrapErr msg e = (adHoc msg) { adHocCause = Just (SomeDiagnostic e) }

-- | Lift any 'Show'-only error into a minimal 'Diagnostic' — the direct
-- analogue of miette's blanket @IntoDiagnostic@ for @std::error::Error@.
-- Deliberately asks for nothing but 'Show': that is exactly the bar
-- @std::error::Error@ itself sets (a @Display@ impl), so this is the direct
-- Haskell analogue, not a weaker substitute for it.
-- | Turn any value with a 'Show' instance into an ad hoc diagnostic whose
-- message is its 'show' output. 'show' is a debugging format, not a message:
-- a 'String' or 'Text' comes out quoted and escaped. Prefer 'fromError' for
-- exceptions, or 'adHoc' for text you already have.
intoDiagnostic :: Show e => e -> AdHocDiagnostic
intoDiagnostic = adHoc . T.pack . show

-- | Turn any 'Exception' into an ad hoc diagnostic whose message is its
-- 'displayException' text.
fromError :: Exception e => e -> AdHocDiagnostic
fromError = adHoc . T.pack . displayException

-- | Lift a caught 'SomeException' into a minimal 'Diagnostic', via its
-- 'displayException' rather than bare 'show' — the message each exception's
-- author already wrote to be user-facing, mirroring miette reading an
-- error's @Display@ impl rather than its @Debug@ one.
fromSomeException :: SomeException -> AdHocDiagnostic
fromSomeException = adHoc . T.pack . displayException

-- | Attach source to an otherwise sourceless diagnostic — the direct
-- analogue of miette's @with_source_code()@: a library raises an error with
-- no source attached (it may have had none in scope), and the application
-- attaches it once it does, after the fact rather than at the error's own
-- construction time. Resolves every given span, degrading (never dropping)
-- any that don't resolve against the given source — the same discipline
-- 'Tadka.Internal.Context.mkContextDegrading' applies everywhere else in
-- this library. Overwrites any context the diagnostic already had: calling
-- this twice keeps only the second source.
withSourceCode :: SourceCode s => s -> [(Span, Maybe (Doc Ann))] -> AdHocDiagnostic -> AdHocDiagnostic
withSourceCode src labels d = d { adHocContext = buildContext src labels }

-- | Attach a diagnostic code.
withCode :: DiagnosticCode -> AdHocDiagnostic -> AdHocDiagnostic
withCode c d = d { adHocCode = Just c }

-- | Set the severity (the default is 'SevError').
withSeverity :: Severity -> AdHocDiagnostic -> AdHocDiagnostic
withSeverity s d = d { adHocSeverity = s }

-- | Attach a help line.
withHelp :: Text -> AdHocDiagnostic -> AdHocDiagnostic
withHelp h d = d { adHocHelp = Just (pretty h) }

-- | Attach a documentation URL.
withUrl :: Url -> AdHocDiagnostic -> AdHocDiagnostic
withUrl u d = d { adHocUrl = Just u }
