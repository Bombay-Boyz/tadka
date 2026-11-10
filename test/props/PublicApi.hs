{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Public functions that no other test exercised: the @diag*@ builders of
-- 'diagnosticSpec' (alone, and all together in a derive), 'withSourceCode',
-- 'fromSomeException', 'withLabelPalette', and 'withoutHighlighter'.
module PublicApi (group) where

import           Control.Exception      (toException)
import qualified Data.List.NonEmpty     as NE
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Language.Haskell.TH    (mkName)

import           Hedgehog

import           RenderUtil
import           Tadka
import           Tadka.Internal.Config  (configHighlighter, configPalette)

once :: PropertyT IO () -> Property
once = withTests 1 . property

-- === The builders, one setting at a time ======================================

prop_scalars :: PropertyT IO ()
prop_scalars = do
  let spec = diagnosticSpec
        [ diagSeverity SevWarning, diagSeverity SevAdvice
        , diagUrl "https://a.example", diagUrl "https://b.example"
        , diagRelated (mkName "r1"), diagRelated (mkName "r2")
        , diagCause (mkName "c1"), diagCause (mkName "c2")
        , diagId (mkName "i1"), diagId (mkName "i2")
        ]
  specSeverity spec === SevAdvice
  specUrl spec      === Just "https://b.example"
  specRelated spec  === Just (mkName "r2")
  specCause spec    === Just (mkName "c2")
  specId spec       === Just (mkName "i2")

prop_appends :: PropertyT IO ()
prop_appends = do
  let spec = diagnosticSpec
        [ diagSecondaryLabel (mkName "a") "x", diagSecondaryLabel (mkName "b") "y"
        , diagLabelCollection (mkName "c") "z", diagLabelCollection (mkName "d") "w"
        , diagSecondaryLabelCollection (mkName "e") "v", diagSecondaryLabelCollection (mkName "f") "u"
        , diagLabelCollectionWith (mkName "g") [| const [] |]
        , diagLabelCollectionWith (mkName "h") [| const [] |]
        , diagSecondaryLabelCollectionWith (mkName "i") [| const [] |]
        ]
  specSecondaryLabelFields spec === [(mkName "a", "x"), (mkName "b", "y")]
  specLabelCollectionFields spec === [(mkName "c", "z"), (mkName "d", "w")]
  specSecondaryLabelCollectionFields spec === [(mkName "e", "v"), (mkName "f", "u")]
  map fst (specLabelCollectionFieldsWith spec) === [mkName "g", mkName "h"]
  map fst (specSecondaryLabelCollectionFieldsWith spec) === [mkName "i"]

-- === Every builder, in a real derive ============================================

data Everything = Everything
  { fuSource :: NamedSource
  , fuNew    :: Span
  , fuOld    :: Span
  , fuMany   :: [Span]
  , fuOther  :: [Span]
  , fuRel    :: [SomeDiagnostic]
  , fuCause  :: Maybe SomeDiagnostic
  , fuId     :: Text
  }

deriveDiagnostic
  (diagnosticSpec
    [ diagMessage [| \_ -> "everything" |]
    , diagSeverity SevAdvice
    , diagUrl "https://example.com/full"
    , diagSource 'fuSource
    , diagLabel 'fuNew "new label"
    , diagSecondaryLabel 'fuOld "old label"
    , diagLabelCollection 'fuMany "many label"
    , diagSecondaryLabelCollection 'fuOther "other label"
    , diagRelated 'fuRel
    , diagCause 'fuCause
    , diagId 'fuId
    ])
  ''Everything

everything :: Everything
everything = Everything
  { fuSource = rightOrErr (mkNamedSource "full.hs" "aaaa\nbbbb\ncccc\ndddd\n")
  , fuNew    = rightOrErr (mkSpan 0 2)
  , fuOld    = rightOrErr (mkSpan 5 2)
  , fuMany   = [rightOrErr (mkSpan 10 2)]
  , fuOther  = [rightOrErr (mkSpan 15 2)]
  , fuRel    = [SomeDiagnostic (adHoc "related diagnostic")]
  , fuCause  = Just (SomeDiagnostic (adHoc "root cause"))
  , fuId     = "full-1"
  }

prop_derivedFull :: PropertyT IO ()
prop_derivedFull = do
  severity everything === SevAdvice
  fmap unUrl (url everything) === Just "https://example.com/full"
  fmap unDiagnosticId (diagnosticId everything) === Just "full-1"
  length (related everything) === 1
  fmap (const ()) (diagnosticCause everything) === Just ()
  let out = renderG plainConfig everything
  mapM_ (\needle -> assert (needle `T.isInfixOf` out))
    [ "everything", "full.hs", "new label", "old label", "many label", "other label"
    , "related diagnostic", "root cause" ]

-- === Per-element text ===========================================================

data PerElement = PerElement
  { peSource :: NamedSource
  , pePrior  :: [(Span, Text)]
  , peLater  :: [(Span, Text)]
  }

deriveDiagnostic
  (diagnosticSpec
    [ diagMessage [| \_ -> "per element" |]
    , diagSource 'peSource
    , diagLabelCollectionWith 'pePrior [| pePrior |]
    , diagSecondaryLabelCollectionWith 'peLater [| peLater |]
    ])
  ''PerElement

perElement :: PerElement
perElement = PerElement
  { peSource = rightOrErr (mkNamedSource "each.hs" "aaaa\nbbbb\ncccc\n")
  , pePrior  = [(rightOrErr (mkSpan 0 2), "first text"), (rightOrErr (mkSpan 5 2), "second text")]
  , peLater  = [(rightOrErr (mkSpan 10 2), "third text")]
  }

prop_perElement :: PropertyT IO ()
prop_perElement = do
  let out = renderG plainConfig perElement
  mapM_ (\needle -> assert (needle `T.isInfixOf` out))
    [ "per element", "each.hs", "first text", "second text", "third text" ]

-- === Ad hoc diagnostics =========================================================

prop_withSourceCode :: PropertyT IO ()
prop_withSourceCode = do
  let src = rightOrErr (mkNamedSource "buf.txt" "alpha beta\n")
      sp  = rightOrErr (mkSpan 6 4)
      d   = withSourceCode src [(sp, Just "this word")] (adHoc "problem")
      out = renderG plainConfig d
  mapM_ (\needle -> assert (needle `T.isInfixOf` out))
    [ "problem", "buf.txt", "alpha beta", "this word" ]

prop_fromSomeException :: PropertyT IO ()
prop_fromSomeException = do
  let out = renderN plainConfig (fromSomeException (toException (userError "oops")))
  assert ("user error (oops)" `T.isInfixOf` out)

-- === Configuration ==============================================================

prop_palette :: PropertyT IO ()
prop_palette = do
  let reversed = NE.reverse (configPalette defaultConfig)
  configPalette (withLabelPalette reversed defaultConfig) === reversed

prop_withoutHighlighter :: PropertyT IO ()
prop_withoutHighlighter = do
  let h   = Highlighter "none" (\_ _ -> Nothing)
      cfg = withHighlighter h defaultConfig
  configHighlighter cfg === Just h
  configHighlighter (withoutHighlighter cfg) === Nothing

-- === The group =================================================================
--
-- Last, because the splices above split the module into declaration groups and
-- a group can only refer to names from the groups before it.

group :: Group
group = Group "Public API without another test"
  [ ("later diag* directives overwrite scalar settings",     once prop_scalars)
  , ("label and collection directives append in order",      once prop_appends)
  , ("every diag* builder works in a derive",                once prop_derivedFull)
  , ("per-element collection labels carry their own text",   once prop_perElement)
  , ("withSourceCode attaches a source to an ad hoc diagnostic", once prop_withSourceCode)
  , ("fromSomeException keeps the exception's message",      once prop_fromSomeException)
  , ("withLabelPalette replaces the palette",                once prop_palette)
  , ("withoutHighlighter removes a highlighter",             once prop_withoutHighlighter)
  ]
