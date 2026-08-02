{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module LabelsWithoutSource where
import Tadka
data E = E { at :: Span } deriving (Show)
-- specLabelFields names a field, but specSourceField is Nothing: with no
-- source to anchor to, contextMethod would emit no `context` method at all
-- and 'at' would be silently dropped rather than rejected.
deriveDiagnostic defaultSpec { specLabelFields = [('at, "here")] } ''E
