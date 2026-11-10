-- | Property test suite entry point. Each phase contributes a Hedgehog 'Group';
-- Main runs them all and fails if any property fails.
module Main (main) where

import           Control.Monad   (unless)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           System.Exit     (exitFailure)

import           Hedgehog        (checkParallel)

import qualified Phase1
import qualified Phase2
import qualified Phase3
import qualified Phase4
import qualified Phase5
import qualified Phase6
import qualified Phase7
import qualified Phase8
import qualified Phase9
import qualified Phase11
import qualified Phase12
import qualified Phase13
import qualified Tabs
import qualified TermColor
import qualified Hyperlink
import qualified LabelCollection
import qualified Labels
import qualified Cause
import qualified Source
import qualified LinePlanSpec
import qualified LayoutSpec
import qualified MultiLine
import qualified EdgeCases
import qualified Unicode
import qualified HighlightSafety
import qualified SourceLaws
import qualified HighlightRender
import qualified SourceRender
import qualified CauseFull
import qualified Bridge
import qualified ContextLines
import qualified PublicApi
import qualified PanicHook

main :: IO ()
main = do
  setLocaleEncoding utf8   -- tadka emits Unicode; be locale-independent
  results <- traverse checkParallel [Phase1.group, Phase2.group, Phase3.group, Phase4.group, Phase5.group, Phase6.group, Phase7.group, Phase8.group, Phase9.group, Phase11.group, Phase12.group, Phase13.group, Tabs.group, TermColor.group, Hyperlink.group, LabelCollection.group, Labels.group, Cause.group, Source.group, LinePlanSpec.group, LayoutSpec.group, MultiLine.group, EdgeCases.group, Unicode.group, HighlightSafety.group, SourceLaws.group, HighlightRender.group, SourceRender.group, CauseFull.group, Bridge.group, ContextLines.group, PublicApi.group, PanicHook.group]
  unless (and results) exitFailure
