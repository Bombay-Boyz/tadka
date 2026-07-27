{-# LANGUAGE OverloadedStrings #-}

-- | Shared Hedgehog generators for arbitrary diagnostics, used by the Phase 5
-- and Phase 6 totality smoke checks so both exercise the same fuel-bounded set.
module GenDiag
  ( GD (..)
  , genGD
  , selfNothing
  , selfJust
  , selfCauseJust
  , selfCauseNothing
  , genScalar
  , genLine
  ) where

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Prettyprinter  (Doc, pretty)

import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           Tadka
import           Tadka.Internal (buildContext)

-- | A generated diagnostic carrier (no 'Show'; use @forAllWith@).
data GD = GD
  { gdMsg  :: Text
  , gdCode :: Maybe DiagnosticCode
  , gdCtx  :: Context
  , gdHelp :: Maybe Text
  , gdUrl  :: Maybe Url
  , gdRel  :: [SomeDiagnostic]
  , gdId   :: Maybe DiagnosticId
  , gdCause :: Maybe SomeDiagnostic
  }

instance Diagnostic GD where
  message      = pretty . gdMsg
  code         = gdCode
  context      = gdCtx
  help         = fmap pretty . gdHelp
  url          = gdUrl
  related      = gdRel
  diagnosticId = gdId
  diagnosticCause = gdCause

-- Scalars spanning ASCII, combining marks, wide CJK, and emoji.
genScalar :: Gen Char
genScalar = Gen.frequency
  [ (6, Gen.filterT (/= '\n') (Gen.enum ' ' '~'))
  , (2, Gen.enum '\x0300' '\x036F')    -- combining marks (width 0)
  , (2, Gen.enum '\x4E00' '\x4E30')    -- CJK (width 2)
  , (1, Gen.enum '\x1F600' '\x1F610')  -- emoji (width 2)
  ]

genLine :: Gen Text
genLine = Gen.text (Range.linear 0 30) genScalar

genCode :: Gen DiagnosticCode
genCode = Gen.element (map mk ["tadka::E0001", "pkg::E4242", "z9::E00000"])
  where mk t = either (error "bad code") id (mkDiagnosticCode t)

genCtx :: Gen Context
genCtx = do
  txt <- Gen.text (Range.linear 0 40) genScalar
  k   <- Gen.int (Range.linear 0 3)
  ls  <- Gen.list (Range.singleton k) (genSpanLabel (T.length txt))
  pure $ case (ls, mkNamedSource "gen.hs" (if T.null txt then "x" else txt)) of
    ([], _)        -> NoContext
    (_, Right src) -> buildContext src ls
    (_, Left _)    -> NoContext

genSpanLabel :: Int -> Gen (Span, Maybe (Doc Ann))
genSpanLabel maxOff = do
  off <- Gen.int (Range.linear 0 (maxOff + 20))  -- may exceed source => stale label
  len <- Gen.int (Range.linear 0 6)
  lbl <- Gen.maybe (pretty <$> Gen.text (Range.linear 1 10) genScalar)
  pure (either (error "bad span") id (mkSpan off len), lbl)

genGD :: Int -> Gen GD
genGD fuel = do
  msg   <- Gen.text (Range.linear 0 20) genScalar
  mcode <- Gen.maybe genCode
  ctx   <- genCtx
  mhelp <- Gen.maybe (Gen.text (Range.linear 1 15) genScalar)
  rel   <- if fuel <= 0
             then pure []
             else Gen.list (Range.linear 0 2) (SomeDiagnostic <$> genGD (fuel - 1))
  mid   <- Gen.maybe (mkDiagnosticId <$> Gen.text (Range.linear 1 6) (Gen.enum 'a' 'z'))
  mcause <- if fuel <= 0
              then pure Nothing
              else Gen.maybe (SomeDiagnostic <$> genGD (fuel - 1))
  pure (GD msg mcode ctx mhelp Nothing rel mid mcause)

-- Self-referential related chains: one detected by id (cycle), one relying on
-- the depth budget (Nothing id).
selfNothing, selfJust :: GD
selfNothing = let d = GD "loops (no id)" Nothing NoContext Nothing Nothing [SomeDiagnostic d] Nothing Nothing in d
selfJust    = let d = GD "loops (id)"    Nothing NoContext Nothing Nothing [SomeDiagnostic d]
                        (Just (mkDiagnosticId "loop")) Nothing in d

-- Self-referential CAUSE chains: one detected by id (cycle), one relying on the
-- depth budget (Nothing id).
selfCauseJust, selfCauseNothing :: GD
selfCauseJust    = let d = GD "cause loops (id)" Nothing NoContext Nothing Nothing []
                            (Just (mkDiagnosticId "cloop")) (Just (SomeDiagnostic d)) in d
selfCauseNothing = let d = GD "cause loops (no id)" Nothing NoContext Nothing Nothing []
                            Nothing (Just (SomeDiagnostic d)) in d
