{-# LANGUAGE OverloadedStrings #-}

-- | A user-defined 'SourceCode' instance, used through the whole pipeline:
-- context building, all three renderers, and a mixed-type multi-source
-- context. The point is that a custom source renders identically to the
-- built-in one holding the same text.
module SourceRender (group) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import qualified Data.Text          as T

import           Hedgehog
import           Prettyprinter      (Doc, pretty)

import           GenDiag            (GD (..))
import           RenderUtil
import           SourceLaws         (LineSource (..))
import           Tadka
import           Tadka.Internal     (buildContext, buildContextMulti)

group :: Group
group = Group "A custom SourceCode through the pipeline"
  [ ("all three renderers show the custom source",            once prop_rendersCustom)
  , ("custom and built-in sources render identically",        once prop_identical)
  , ("a multi-source context can mix source types",           once prop_mixed)
  , ("a label past the end of a custom source degrades",      once prop_stale)
  ]

once :: PropertyT IO () -> Property
once = withTests 1 . property

lineSource :: LineSource
lineSource = LineSource "virtual.buf" ["let x = 1", "let y = 2"]

namedSource :: NamedSource
namedSource = rightOrErr (mkNamedSource "virtual.buf" "let x = 1\nlet y = 2")

hereLabel :: Maybe (Doc Ann)
hereLabel = Just (pretty ("here" :: Text))

span4 :: Span
span4 = rightOrErr (mkSpan 4 1)

diagnosing :: SourceCode s => s -> GD
diagnosing s =
  GD "boom" Nothing (buildContext s [(span4, hereLabel)]) Nothing Nothing [] Nothing Nothing

prop_rendersCustom :: PropertyT IO ()
prop_rendersCustom = do
  let d = diagnosing lineSource
      g = renderG plainConfig d
  assert ("virtual.buf" `T.isInfixOf` g)
  assert ("let x = 1" `T.isInfixOf` g)
  assert ("virtual.buf" `T.isInfixOf` renderN plainConfig d)
  assert ("virtual.buf" `T.isInfixOf` renderJ plainConfig d)

prop_identical :: PropertyT IO ()
prop_identical = do
  let custom = diagnosing lineSource
      builtIn = diagnosing namedSource
  renderG plainConfig custom === renderG plainConfig builtIn
  renderN plainConfig custom === renderN plainConfig builtIn
  renderJ plainConfig custom === renderJ plainConfig builtIn

prop_mixed :: PropertyT IO ()
prop_mixed = do
  let other = rightOrErr (mkNamedSource "other.hs" "main = pure ()\n")
      ctx   = buildContextMulti
                (  (someSource lineSource, [(span4, Primary, hereLabel)])
              :| [(someSource other,      [(rightOrErr (mkSpan 0 4), Secondary, hereLabel)])])
      d     = GD "boom" Nothing ctx Nothing Nothing [] Nothing Nothing
      g     = renderG plainConfig d
  assert ("virtual.buf" `T.isInfixOf` g)
  assert ("other.hs" `T.isInfixOf` g)

prop_stale :: PropertyT IO ()
prop_stale = do
  let tiny = LineSource "short.buf" ["ab"]
      far  = rightOrErr (mkSpan 50 3)
      d    = GD "boom" Nothing (buildContext tiny [(far, hereLabel)]) Nothing Nothing [] Nothing Nothing
  assert ("boom" `T.isInfixOf` renderG plainConfig d)
  assert ("boom" `T.isInfixOf` renderN plainConfig d)
  assert ("boom" `T.isInfixOf` renderJ plainConfig d)
