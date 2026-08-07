{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Concrete diagnostics for the graphical golden fixtures (spec Phase 5).
-- Offsets are computed from prefix lengths so the labelled spans are exact.
module Fixtures
  ( fixtures
  , narratableFixtures
  , jsonFixtures
  , contextFixtures
  , generatedSource
  , single
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Prettyprinter (Doc, pretty)
import           Language.Haskell.TH (litE, pprint, stringL)

import           Tadka
import           Tadka.Internal (buildContext, buildContextMulti, buildContextWith)

-- A general-purpose Diagnostic carrier for fixtures.
data Fix = Fix
  { fMsg  :: Text
  , fCode :: Maybe DiagnosticCode
  , fCtx  :: Context
  , fHelp :: Maybe Text
  , fUrl  :: Maybe Url
  , fRel  :: [SomeDiagnostic]
  , fId   :: Maybe DiagnosticId
  }

instance Diagnostic Fix where
  message      = pretty . fMsg
  code         = fCode
  context      = fCtx
  help         = fmap pretty . fHelp
  url          = fUrl
  related      = fRel
  diagnosticId = fId

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

mkCode :: Text -> DiagnosticCode
mkCode = rightOrErr . mkDiagnosticCode

label :: Text -> Maybe (Doc Ann)
label t = Just (pretty t)

-- 1. Single label -----------------------------------------------------------
src1 :: NamedSource
src1 = rightOrErr (mkNamedSource "example.hs" "line1\nline2\nlet x = foo + 1\n")

-- A span that crosses lines, to exercise the connector lanes (Phase III).
multiLine :: Fix
multiLine = Fix
  { fMsg  = "unterminated function call"
  , fCode = Just (mkCode "tadka::E0300")
  , fCtx  = buildContext srcCall
              [ (rightOrErr (mkSpan 8 22), label "opened here, never closed") ]
  , fHelp = Just "add a closing `)`"
  , fUrl  = Nothing, fRel = [], fId = Nothing
  }

srcCall :: NamedSource
srcCall = rightOrErr (mkNamedSource "call.hs" "let x = foo(\n    bar,\n    baz)\n")

-- Labels far apart, to exercise context lines + gap elision (rendered with
-- withContextLines 1 in the golden harness).
farApart :: Fix
farApart = Fix
  { fMsg  = "two distant problems"
  , fCode = Just (mkCode "tadka::E0200")
  , fCtx  = buildContextWith srcTen
              [ (rightOrErr (mkSpan 4 2),  Secondary, label "first")
              , (rightOrErr (mkSpan 40 4), Primary,   label "second")
              ]
  , fHelp = Nothing, fUrl = Nothing, fRel = [], fId = Nothing
  }

srcTen :: NamedSource
srcTen = rightOrErr (mkNamedSource "big.hs"
  "one aaa\ntwo bbb\nthree c\nfour dd\nfive ee\nsix fff\nseven g\neight h\nnine ii\nten jjj\n")

-- A minimal chainable diagnostic for exercising the cause chain (Part B).
data CauseNode = CauseNode Text (Maybe DiagnosticCode) (Maybe SomeDiagnostic)

instance Diagnostic CauseNode where
  message (CauseNode m _ _) = pretty m
  code    (CauseNode _ c _) = c
  diagnosticCause (CauseNode _ _ mc) = mc

-- E0100 caused by E0042 caused by an uncoded leaf.
withCause :: CauseNode
withCause = CauseNode "failed to compile `Main`" (Just (mkCode "tadka::E0100"))
  (Just (SomeDiagnostic (CauseNode "type mismatch in `foo`" (Just (mkCode "tadka::E0042"))
    (Just (SomeDiagnostic (CauseNode "unbound variable `x`" Nothing Nothing))))))

single :: Fix
single = Fix
  { fMsg  = "undefined variable `foo`"
  , fCode = Just (mkCode "tadka::E0001")
  , fCtx  = buildContext src1
              [ (rightOrErr (mkSpan (T.length "line1\nline2\nlet x = ") 3), label "not in scope") ]
  , fHelp = Just "did you mean `bar`?"
  , fUrl  = Just (rightOrErr (mkUrl "https://example.org/errors/E0001"))
  , fRel  = []
  , fId   = Nothing
  }

-- 2. Multiple labels + related ----------------------------------------------
src2 :: NamedSource
src2 = rightOrErr (mkNamedSource "example.hs"
         "addOne :: Int -> Int\naddOne x = x\nresult = addOne \"hi\"\n")

related43 :: Fix
related43 = Fix
  { fMsg  = "conflicting instance defined here"
  , fCode = Just (mkCode "tadka::E0043")
  , fCtx  = buildContext (rightOrErr (mkNamedSource "Prelude.hs" "instance Num String where ...\n"))
              [ (rightOrErr (mkSpan 0 8), label "conflicting instance") ]
  , fHelp = Nothing, fUrl = Nothing, fRel = [], fId = Nothing
  }

multi :: Fix
multi = Fix
  { fMsg  = "type mismatch"
  , fCode = Just (mkCode "tadka::E0042")
  , fCtx  = buildContextWith src2
              [ (rightOrErr (mkSpan (T.length "addOne :: ") 3), Secondary, label "expected because of this")
              , (rightOrErr (mkSpan (T.length "addOne :: Int -> Int\naddOne x = x\nresult = addOne ") 4),
                 Primary, label "found `String`, expected `Int`")
              ]
  , fHelp = Just "convert with `show` or change the annotation"
  , fUrl  = Nothing
  , fRel  = [SomeDiagnostic related43]
  , fId   = Nothing
  }

-- 3. Degraded (stale) label -------------------------------------------------
degraded :: Fix
degraded = Fix
  { fMsg  = "undefined variable `foo`"
  , fCode = Just (mkCode "tadka::E0001")
  , fCtx  = buildContext (rightOrErr (mkNamedSource "example.hs" "let x = 1\n"))
              [ (rightOrErr (mkSpan 100 3), label "not in scope") ]  -- out of bounds -> stale
  , fHelp = Just "did you mean `bar`?"
  , fUrl  = Nothing, fRel = [], fId = Nothing
  }

-- 4. Cycle-omitted related --------------------------------------------------
cyc :: Fix
cyc =
  let d = Fix
            { fMsg  = "conflicting instance defined here"
            , fCode = Just (mkCode "tadka::E0043")
            , fCtx  = buildContext (rightOrErr (mkNamedSource "Prelude.hs" "instance Num String where ...\n"))
                        [ (rightOrErr (mkSpan 0 8), label "conflicting instance") ]
            , fHelp = Nothing, fUrl = Nothing
            , fRel  = [SomeDiagnostic d]                 -- self-reference => cycle
            , fId   = Just (mkDiagnosticId "e0043")
            }
  in d

-- 5. Cross-file (Phase 12) -----------------------------------------------
-- Two sources, each contributing one label: the shape a real "this import
-- disagrees with that definition" diagnostic takes once a Context can span
-- more than one file. Exercises all three renderers via the shared fixture
-- lists below, same as 'single'/'withCause' already do.
srcModA :: NamedSource
srcModA = rightOrErr (mkNamedSource "ModuleA.hs" "import ModuleB (widget)\nmain = widget\n")

srcModB :: NamedSource
srcModB = rightOrErr (mkNamedSource "ModuleB.hs" "widget :: Int\nwidget = 42\n")

crossFile :: Fix
crossFile = Fix
  { fMsg  = "type mismatch across modules"
  , fCode = Just (mkCode "tadka::E0500")
  , fCtx  = buildContextMulti
              ( ( srcModA
                , [ (rightOrErr (mkSpan (T.length "import ModuleB (") 6), Secondary,
                     label "imported here")
                  ]
                )
              :| [ ( srcModB
                   , [ (rightOrErr (mkSpan 0 6), Primary, label "defined here as `Int`") ]
                   )
                 ]
              )
  , fHelp = Just "the two declarations must agree"
  , fUrl  = Nothing, fRel = [], fId = Nothing
  }

fixtures :: [(String, SomeDiagnostic)]
fixtures =
  [ ("single-label",  SomeDiagnostic single)
  , ("multi-label",   SomeDiagnostic multi)
  , ("degraded",      SomeDiagnostic degraded)
  , ("cycle-omitted", SomeDiagnostic cyc)
  , ("tab-indented",  SomeDiagnostic tabIndented)
  , ("with-cause",    SomeDiagnostic withCause)
  , ("multi-line",     SomeDiagnostic multiLine)
  , ("cross-file",     SomeDiagnostic crossFile)
  ]

-- Tab-indented source: the caret must align under the tab-EXPANDED position.
tabIndented :: Fix
tabIndented = Fix
  { fMsg  = "undefined variable `foo`"
  , fCode = Just (mkCode "tadka::E0001")
  , fCtx  = buildContext
              (rightOrErr (mkNamedSource "tab.hs" "func x =\n\t  return foo\n"))
              [ (rightOrErr (mkSpan (T.length "func x =\n\t  return ") 3), label "not in scope") ]
  , fHelp = Nothing, fUrl = Nothing, fRel = [], fId = Nothing
  }

-- Narratable fixtures ------------------------------------------------------
-- A related chain deeper than the depth limit the golden runner renders at,
-- so the prose truncation marker fires.
relB :: Fix
relB = Fix "second related problem" (Just (mkCode "pkg::E1002"))
           NoContext Nothing Nothing [] Nothing

relA :: Fix
relA = Fix "first related problem" (Just (mkCode "pkg::E1001"))
           NoContext Nothing Nothing [SomeDiagnostic relB] Nothing

truncatedRoot :: Fix
truncatedRoot = Fix
  { fMsg  = "top-level problem"
  , fCode = Just (mkCode "pkg::E1000")
  , fCtx  = buildContext src1
              [ (rightOrErr (mkSpan (T.length "line1\nline2\nlet x = ") 3), label "here") ]
  , fHelp = Just "see the related items"
  , fUrl  = Nothing
  , fRel  = [SomeDiagnostic relA]
  , fId   = Nothing
  }

narratableFixtures :: [(String, SomeDiagnostic)]
narratableFixtures =
  [ ("narr-single",    SomeDiagnostic single)
  , ("narr-truncated", SomeDiagnostic truncatedRoot)
  , ("narr-cause",     SomeDiagnostic withCause)
  , ("narr-cross-file", SomeDiagnostic crossFile)
  ]

-- JSON fixtures (rendered at depth limit 1 by the runner): single (matches the
-- vision example), cycle (cycleOmitted flag), truncated (nested truncated flag).
contextFixtures :: [(String, SomeDiagnostic)]
contextFixtures =
  [ ("context-elision", SomeDiagnostic farApart) ]

jsonFixtures :: [(String, SomeDiagnostic)]
jsonFixtures =
  [ ("json-single",    SomeDiagnostic single)
  , ("json-cycle",     SomeDiagnostic cyc)
  , ("json-truncated", SomeDiagnostic truncatedRoot)
  , ("json-cause",     SomeDiagnostic withCause)
  , ("json-cross-file", SomeDiagnostic crossFile)
  ]


data ParseError = UnexpectedToken
  { errSource :: NamedSource
  , got       :: Text
  , expected  :: Text
  , at        :: Span
  }
  deriving (Show)

$(do
    decs <- deriveDiagnostic defaultSpec
              { specCode        = Just "tadka::E0001"
              , specHelp        = Just "did you forget a semicolon?"
              , specSourceField = Just 'errSource
              , specLabelFields = [('at, "unexpected token here")]
              }
              ''ParseError
    dump <- [d| generatedSource :: String
                generatedSource = $(litE (stringL (pprint decs))) |]
    pure (decs ++ dump))
