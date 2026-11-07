{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Several labels in one diagnostic: a primary and a secondary label, a
-- runtime-sized collection of labels, and the same thing written by hand.
module Examples.Labels (examples) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import           Prettyprinter      (pretty)

import           Example
import           Tadka

-- 1. A primary label and a secondary label, derived.

data ShadowedBinding = ShadowedBinding
  { sbSource :: NamedSource
  , sbNewAt  :: Span
  , sbOldAt  :: Span
  , sbName   :: Text
  }

deriveDiagnostic defaultSpec
  { specMessage =
      Just [| \e -> "`" <> pretty (sbName e) <> "` shadows an earlier binding" |]
  , specSourceField          = Just 'sbSource
  , specLabelFields          = [('sbNewAt, "new binding here")]
  , specSecondaryLabelFields = [('sbOldAt, "previous binding was here")]
  }
  ''ShadowedBinding

-- 2. A fixed label plus a collection whose length is only known at runtime.

data MultipleDefinitions = MultipleDefinitions
  { mdSource :: NamedSource
  , mdName   :: Text
  , mdLatest :: Span
  , mdPrior  :: [Span]
  }

deriveDiagnostic defaultSpec
  { specMessage =
      Just [| \e -> "`" <> pretty (mdName e) <> "` is defined more than once" |]
  , specSourceField                    = Just 'mdSource
  , specLabelFields                    = [('mdLatest, "latest definition")]
  , specSecondaryLabelCollectionFields = [('mdPrior, "also defined here")]
  }
  ''MultipleDefinitions

-- 3. The same labels written by hand.

data Clash = Clash NamedSource Span Span

instance Diagnostic Clash where
  message _ = "conflicting definitions"
  context (Clash src first second) =
    mkContextDegrading src
      (  Labeled second Primary (Just "redefined here")
      :| [Labeled first Secondary (Just "first defined here")])

shadowed :: ShadowedBinding
shadowed = ShadowedBinding src newAt oldAt "x"
  where
    src   = either (error . show) id (mkNamedSource "demo.hs" "let x = 1\nlet x = 2\n")
    newAt = either (error . show) id (mkSpan 14 1)
    oldAt = either (error . show) id (mkSpan 4 1)

multiple :: MultipleDefinitions
multiple = MultipleDefinitions src "x" latest [first, second]
  where
    src    = either (error . show) id (mkNamedSource "demo.hs" "x = 1\nx = 2\nx = 3\n")
    latest = either (error . show) id (mkSpan 12 1)
    first  = either (error . show) id (mkSpan 0 1)
    second = either (error . show) id (mkSpan 6 1)

clash :: Clash
clash = Clash src first second
  where
    src    = either (error . show) id (mkNamedSource "demo.hs" "x = 1\nx = 2\n")
    first  = either (error . show) id (mkSpan 0 1)
    second = either (error . show) id (mkSpan 6 1)

examples :: [Example]
examples =
  [ Example "labels-derive" (renderPlain shadowed)
      [ "`x` shadows an earlier binding", "new binding here", "previous binding was here" ]
  , Example "labels-collection" (renderPlain multiple)
      [ "`x` is defined more than once", "latest definition", "also defined here" ]
  , Example "labels-manual" (renderPlain clash)
      [ "conflicting definitions", "redefined here", "first defined here" ]
  ]
