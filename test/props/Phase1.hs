{-# LANGUAGE OverloadedStrings #-}

-- | Phase 1 properties: primitive types, smart constructors, width lookups.
module Phase1 (group) where

import           Data.Char           (isAsciiLower, isDigit)
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Hedgehog
                   (Gen, Group (..), Property, assert, forAll, property,
                    withTests, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Tadka
import           Tadka.Internal.Types (LengthError (..), OffsetError (..), mkLength,
                                             mkOffset, unLength, unOffset)
import           Tadka.Internal.Width
                   (GBProp (..), charWidth, graphemeBreakProperty,
                    isExtendedPictographic, textWidth)

group :: Group
group = Group "Phase 1 - primitive types & width"
  [ ("mkOffset rejects exactly negatives",                  prop_mkOffset)
  , ("mkLength rejects exactly negatives",                  prop_mkLength)
  , ("mkNamedSource: empty name rejected; else round-trip", prop_mkNamedSource)
  , ("mkDiagnosticCode agrees with grammar oracle",         prop_mkDiagnosticCode)
  , ("mkDiagnosticId is total and round-trips",             prop_mkDiagnosticId)
  , ("mkUrl accepts absolute URIs only",                    prop_mkUrl)
  , ("Tadka.Internal.Width point lookups",                  prop_width)
  ]

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-100000) 100000)

genText :: Gen Text
genText = Gen.text (Range.linear 0 30) Gen.unicode

genCodeish :: Gen Text
genCodeish = Gen.choice [genValidCode, genNoisyCode]
  where
    genValidCode = do
      ns   <- genNamespace
      digs <- Gen.text (Range.linear 4 8) Gen.digit
      pure (ns <> "::E" <> digs)
    genNamespace = do
      c0 <- Gen.lower
      cs <- Gen.text (Range.linear 0 6)
              (Gen.choice [Gen.lower, Gen.digit, Gen.constant '_'])
      pure (T.cons c0 cs)
    genNoisyCode =
      Gen.text (Range.linear 0 16)
        (Gen.choice (map Gen.constant "abcdeEZ0123:_# "))

prop_mkOffset :: Property
prop_mkOffset = property $ do
  n <- forAll genInt
  case mkOffset n of
    Left (NegativeOffset m) -> do assert (n < 0);  m === n
    Right o                 -> do assert (n >= 0); unOffset o === n

prop_mkLength :: Property
prop_mkLength = property $ do
  n <- forAll genInt
  case mkLength n of
    Left (NegativeLength m) -> do assert (n < 0);  m === n
    Right l                 -> do assert (n >= 0); unLength l === n

prop_mkNamedSource :: Property
prop_mkNamedSource = property $ do
  name <- forAll genText
  txt  <- forAll genText
  case mkNamedSource name txt of
    Left EmptySourceName -> T.null name === True
    Right ns             -> do
      assert (not (T.null name))
      sourceName ns === name
      sourceText ns === txt

oracleCode :: Text -> Bool
oracleCode t = case T.splitOn "::" t of
  [ns, body] -> okNs (T.unpack ns) && okBody (T.unpack body)
  _          -> False
  where
    okNs []       = False
    okNs (c : cs) = isAsciiLower c
                      && all (\x -> isAsciiLower x || isDigit x || x == '_') cs
    okBody ('E' : ds) = length ds >= 4 && all isDigit ds
    okBody _          = False

prop_mkDiagnosticCode :: Property
prop_mkDiagnosticCode = property $ do
  t <- forAll genCodeish
  case mkDiagnosticCode t of
    Right c                -> do oracleCode t === True; unDiagnosticCode c === t
    Left EmptyCode         -> T.null t === True
    Left (MalformedCode m) -> do m === t
                                 assert (not (T.null t))
                                 oracleCode t === False

prop_mkDiagnosticId :: Property
prop_mkDiagnosticId = property $ do
  t <- forAll genText
  unDiagnosticId (mkDiagnosticId t) === t

urlCases :: [(Text, Bool)]
urlCases =
  [ ("https://example.org/errors/E0001", True)
  , ("http://a.example/",                True)
  , ("ftp://host/path",                  True)
  , ("mailto:person@example.org",        True)
  , ("",                                 False)
  , ("example.org",                      False)
  , ("/relative/path",                   False)
  , ("not a url",                        False)
  , ("http://x.example/#fragment",       False)
  ]

prop_mkUrl :: Property
prop_mkUrl = property $ do
  (input, valid) <- forAll (Gen.choice (map Gen.constant urlCases))
  case mkUrl input of
    Right u                 -> do assert valid; unUrl u === input
    Left EmptyUrl           -> T.null input === True
    Left (NotAbsoluteUri m) -> do assert (not valid); m === input

prop_width :: Property
prop_width = withTests 1 . property $ do
  charWidth 'A'        === 1
  charWidth ' '        === 1
  charWidth '\x0301'   === 0
  charWidth '\xAC00'   === 2
  charWidth '\xD55C'   === 2
  charWidth '\x4E00'   === 2
  textWidth "AB"       === 2
  textWidth "a\x0301"  === 1
  textWidth "\xD55C\xAC00\xC5B4" === 6
  graphemeBreakProperty '\r'      === GBCR
  graphemeBreakProperty '\n'      === GBLF
  graphemeBreakProperty '\x200D'  === GBZWJ
  graphemeBreakProperty 'A'       === GBOther
  assert (isExtendedPictographic '\x1F600')
  assert (not (isExtendedPictographic 'A'))
