-- | A renderer-agnostic annotation vocabulary for diagnostic prose (vision §3).
--
-- @message@, @help@, and label text are all @Doc Ann@ — never @Doc AnsiStyle@ —
-- so the semantic content an author writes (a keyword, a filename, emphasis) is
-- decoupled from how any particular renderer displays it. Each renderer
-- interprets 'Ann' at its own boundary (@toAnsiStyle@ in Phase 5,
-- @toProseMarker@ in Phase 6; the JSON handler discards it in Phase 7).
--
-- Scheduling note: the spec lists 'Ann' under Phase 3, but Phase 2's 'Labeled'
-- and @buildContext@ already need @Doc Ann@ in their signatures. Since 'Ann' is
-- a dependency-free leaf type, it is defined here in Phase 2; Phase 3 adds only
-- the renderer-boundary interpreters, not the type.
--
-- No compatibility guarantee.
module Tadka.Internal.Ann
  ( Ann (..)
  ) where

-- | The closed set of semantic annotations. Deliberately does /not/ grow a
-- per-label-index constructor: which colour the second underline in a
-- multi-label graphical render gets is a positional rendering decision that
-- belongs to the graphical handler (§7), not to author-written content.
data Ann
  = AnnEmphasis
  | AnnCode
  | AnnFilename
  | AnnKeyword
  deriving (Eq, Show, Enum, Bounded)
