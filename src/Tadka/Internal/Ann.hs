
-- No compatibility guarantee.
module Tadka.Internal.Ann
  ( Ann (..)
  ) where


data Ann
  = AnnEmphasis
  | AnnCode
  | AnnFilename
  | AnnKeyword
  deriving (Eq, Show, Enum, Bounded)
