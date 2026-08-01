-- | Plain, exported, independently-testable functions that both the derive
-- macro ("Tadka.Internal.TH", Phase 8) and hand-written 'Tadka.Diagnostic'
-- instances call.
--
-- __The @deriveDiagnostic@ discipline (vision §6).__ Every method body the
-- splice generates must be a direct, unmodified call to a function that also
-- lives in this module's export list (or a class default), so a manual instance
-- can invoke the exact same code path. If a future field needs logic the
-- current surface can't express as a plain call, the fix is to extract a new
-- plain function here first — never to add logic inside the @Q@ splice. This is
-- a code-review convention, not type-enforced; a CI grep over the generated
-- golden fixture backstops it. The one exception is the default @message@
-- (@pretty . show@), which has no manual-instance equivalent by definition.
--
-- No compatibility guarantee.
module Tadka.Internal
  ( unsafeDiagnosticCode
  , unsafeUrl
  , buildContext
  , buildContextWith
  , buildContextMulti
  , mkDiagnosticId
  ) where

import Tadka.Internal.Context (buildContext, buildContextMulti, buildContextWith)
import Tadka.Internal.Types   (mkDiagnosticId, unsafeDiagnosticCode, unsafeUrl)
