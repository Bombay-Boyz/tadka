{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Terminal-capability resolution (pure) and ANSI colour application
-- (post-v1 hardening). The resolver proofs pin exactly how @Auto@ modes become
-- concrete; the colour proofs pin that colour adds only ANSI (and a uniform
-- caret glyph), never structural change — so plain output stays plain.
module TermColor (group) where

import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Prettyprinter                      (LayoutOptions (..),
                                                     PageWidth (Unbounded), layoutPretty)
import           Prettyprinter.Render.Text          (renderStrict)

import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

import           GenDiag                            (genGD)
import           Tadka
import           Tadka.Internal                     (buildContext)
import           Tadka.Internal.Config              (configPalette, configRelatedDepth,
                                                     configTabWidth, configTarget,
                                                     configColorMode, configUnicodeMode,
                                                     defaultPalette)
import           Tadka.Internal.Terminal            (TerminalCaps (..), resolveColor,
                                                     resolveConfig, resolveUnicode)

group :: Group
group = Group "Terminal detection & colour"
  [ ("explicit colour modes pass through",        prop_colorPassthrough)
  , ("explicit Unicode modes pass through",       prop_unicodePassthrough)
  , ("auto colour: NO_COLOR always wins",         prop_noColorWins)
  , ("auto colour: force beats tty",              prop_forceColor)
  , ("auto colour: else follows tty",             prop_ttyColor)
  , ("auto Unicode follows the locale",           prop_autoUnicode)
  , ("resolveConfig eliminates every Auto",       prop_noAutoAfter)
  , ("resolveConfig is idempotent",               prop_idempotent)
  , ("resolveConfig touches only the two modes",  prop_preservesRest)
  , ("ColorNever emits no ANSI",                  prop_neverNoEsc)
  , ("ColorAlways emits ANSI for labels",         prop_alwaysHasEsc)
  , ("colour changes only ANSI + caret glyph",    prop_structureInvariant)
  ]

-- === generators ===========================================================

genCaps :: Gen TerminalCaps
genCaps = TerminalCaps <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool

genColor :: Gen ColorMode
genColor = Gen.element [ColorAuto, ColorAlways, ColorNever]

genUnicode :: Gen UnicodeMode
genUnicode = Gen.element [UnicodeAuto, UnicodeAlways, UnicodeAscii]

-- === pure resolution proofs ===============================================

prop_colorPassthrough :: Property
prop_colorPassthrough = property $ do
  caps <- forAll genCaps
  resolveColor caps ColorAlways === ColorAlways
  resolveColor caps ColorNever  === ColorNever

prop_unicodePassthrough :: Property
prop_unicodePassthrough = property $ do
  caps <- forAll genCaps
  resolveUnicode caps UnicodeAlways === UnicodeAlways
  resolveUnicode caps UnicodeAscii  === UnicodeAscii

prop_noColorWins :: Property
prop_noColorWins = property $ do
  caps <- forAll (fmap (\c -> c { capNoColor = True }) genCaps)
  resolveColor caps ColorAuto === ColorNever

prop_forceColor :: Property
prop_forceColor = property $ do
  caps <- forAll (fmap (\c -> c { capNoColor = False, capForceColor = True }) genCaps)
  resolveColor caps ColorAuto === ColorAlways

prop_ttyColor :: Property
prop_ttyColor = property $ do
  caps0 <- forAll genCaps
  let caps = caps0 { capNoColor = False, capForceColor = False }
  resolveColor caps ColorAuto === (if capIsTerminal caps then ColorAlways else ColorNever)

prop_autoUnicode :: Property
prop_autoUnicode = property $ do
  caps <- forAll genCaps
  resolveUnicode caps UnicodeAuto === (if capUnicode caps then UnicodeAlways else UnicodeAscii)

cfgWith :: ColorMode -> UnicodeMode -> Config
cfgWith cm um =
  withRelatedDepthLimit 3 . withTabWidth 7 . withTarget TJson
    . withColorMode cm . withUnicodeMode um $ defaultConfig

prop_noAutoAfter :: Property
prop_noAutoAfter = property $ do
  caps <- forAll genCaps
  cm   <- forAll genColor
  um   <- forAll genUnicode
  let r = resolveConfig caps (cfgWith cm um)
  assert (configColorMode r `elem` [ColorAlways, ColorNever])
  assert (configUnicodeMode r `elem` [UnicodeAlways, UnicodeAscii])

prop_idempotent :: Property
prop_idempotent = property $ do
  caps <- forAll genCaps
  cm   <- forAll genColor
  um   <- forAll genUnicode
  let c1 = resolveConfig caps (cfgWith cm um)
      c2 = resolveConfig caps c1
  configColorMode c2   === configColorMode c1
  configUnicodeMode c2 === configUnicodeMode c1

prop_preservesRest :: Property
prop_preservesRest = property $ do
  caps <- forAll genCaps
  cm   <- forAll genColor
  um   <- forAll genUnicode
  let r = resolveConfig caps (cfgWith cm um)
  configRelatedDepth r === 3
  configTabWidth r     === 7
  configTarget r       === Just TJson
  assert (configPalette r == defaultPalette)

-- === colour application proofs =============================================

renderGfx :: Diagnostic e => ColorMode -> e -> Text
renderGfx cm e =
  case selectRenderer (withColorMode cm (withUnicodeMode UnicodeAlways (withTarget TGraphical defaultConfig))) of
    SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
    _                            -> ""

esc :: Char
esc = '\ESC'

-- Drop ANSI CSI (…m) sequences.
stripEsc :: Text -> Text
stripEsc = T.pack . go . T.unpack
  where
    go [] = []
    go (c : '[' : rest)
      | c == esc  = go (drop 1 (dropWhile (/= 'm') rest))
    go (c : rest) = c : go rest

-- Normalise the cycling underline glyphs to '^' (safe only where content has no
-- '~'/'-'; the fixture below is chosen that way).
normHats :: Text -> Text
normHats = T.map (\c -> if c == '~' || c == '-' then '^' else c)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- Two labels on one line; label text/source deliberately free of '~'/'-'.
data ColorFix = ColorFix
instance Diagnostic ColorFix where
  message _ = "type mismatch"
  context _ = buildContext (rightOrErr (mkNamedSource "f.hs" "let a = bb"))
                [ (rightOrErr (mkSpan 4 1), Just "first thing")
                , (rightOrErr (mkSpan 8 2), Just "second thing")
                ]

prop_neverNoEsc :: Property
prop_neverNoEsc = property $ do
  d <- forAllWith (const "<generated diagnostic>") (genGD =<< Gen.int (Range.linear 0 3))
  assert (not (T.any (== esc) (renderGfx ColorNever d)))

prop_alwaysHasEsc :: Property
prop_alwaysHasEsc = withTests 1 . property $
  assert (T.any (== esc) (renderGfx ColorAlways ColorFix))

prop_structureInvariant :: Property
prop_structureInvariant = withTests 1 . property $
  stripEsc (renderGfx ColorAlways ColorFix) === normHats (renderGfx ColorNever ColorFix)
