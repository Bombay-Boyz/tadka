-- | Namespace reservation for one-directional interop adapters.
--
-- The concrete adapters — @Tadka.Internal.Interop.Megaparsec@,
-- @.Attoparsec@, and @.GHC@ (SrcSpan) — arrive in Phase 10, each carrying its
-- own upstream dependency. Those deps are deliberately excluded from the
-- Phase 0 dependency pin set (vision Infrastructure; spec Phase 10), so the
-- adapter modules are created when their deps are, not before. No interop
-- module is ever a dependency of a core module.
--
-- No compatibility guarantee. Phase 0: namespace reservation only.
module Tadka.Internal.Interop
  ( -- * Populated in Phase 10.
  ) where
