{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | OSC 8 hyperlinks (post-v1 hardening item 1). The resolver proofs pin
-- exactly how 'HyperlinkAuto' becomes concrete, one-for-one with
-- "TermColor"'s colour proofs; the render proofs pin that wrapping touches
-- only the @= see:@ URL and nothing else — so a diagnostic with no URL is
-- byte-identical whether hyperlinks are on or off, and 'HyperlinkNever'
-- output is always the escape-free baseline regardless of what the message,
-- help text, or URL contain.
--
-- Deliberately independent of "GenDiag"/"Fixtures": a small dedicated carrier
-- and generator here keep this module's coverage from being perturbed by an
-- unrelated change to generators several other suites also depend on.
module Hyperlink (group) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Prettyprinter               (pretty)
import           Prettyprinter.Render.Text  (renderStrict)
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty)

import           Hedgehog                   (Group (..), Gen, Property, forAll, property,
                                             withTests, (===), assert)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Tadka
import           Tadka.Internal.Config       (configHyperlinkMode)
import           Tadka.Internal.Terminal     (TerminalCaps (..), resolveConfig, resolveHyperlink)

group :: Group
group = Group "OSC 8 hyperlinks"
  [ ("explicit hyperlink modes pass through",                prop_explicitPassthrough)
  , ("auto hyperlink: NO_HYPERLINK always wins",             prop_noHyperlinkWins)
  , ("auto hyperlink: force beats tty",                      prop_forceHyperlink)
  , ("auto hyperlink: else follows tty",                     prop_ttyHyperlink)
  , ("resolveConfig eliminates HyperlinkAuto",               prop_noAutoAfter)
  , ("resolveConfig is idempotent on hyperlink mode",        prop_idempotentHyperlink)
  , ("HyperlinkNever never emits an escape",                 prop_neverNoEsc)
  , ("HyperlinkAlways wraps a present URL in OSC 8",         prop_alwaysWrapsUrl)
  , ("no URL: Always and Never render identically",         prop_noUrlNoDifference)
  , ("the wrap adds only the OSC 8 escape, nothing else",    prop_structureInvariant)
  , ("unresolved Auto behaves like Always (mirrors colour)", prop_autoUnresolvedWraps)
  , ("a raw ESC in the message is neutralised, not leaked",  prop_messageEscNeverLeaks)
  ]

-- === generators ============================================================

genCaps :: Gen TerminalCaps
genCaps = TerminalCaps <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool

genHyperlinkMode :: Gen HyperlinkMode
genHyperlinkMode = Gen.element [HyperlinkAuto, HyperlinkAlways, HyperlinkNever]

-- A representative sample of the absolute URIs 'mkUrl' accepts: ordinary
-- https, an IPv6 host, a non-http scheme, userinfo + port + path params,
-- percent-encoding, a bare query string, and the shortest legal form (a
-- scheme with an opaque, non-hierarchical part). Verified individually
-- against 'mkUrl' while writing this module; kept as a fixed set (rather than
-- a from-scratch URI generator) so every case here is independently known
-- valid.
genUrlText :: Gen Text
genUrlText = Gen.element
  [ "https://example.org/errors/E0001"
  , "https://[::1]:8080/path"
  , "mailto:foo@example.com"
  , "a:b"
  , "https://example.com/a%20b"
  , "https://user:pass@host.example/path;p=1"
  , "urn:isbn:0451450523"
  , "https://example.com/x?y=1"
  , "https://example.com:65535/"
  , "file:///etc/passwd"
  ]

genUrl :: Gen Url
genUrl = mkUrlOrErr <$> genUrlText
  where mkUrlOrErr t = either (\e -> error ("Hyperlink.genUrl: " <> show e)) id (mkUrl t)

-- ASCII plus a literal ESC, so properties can confirm the escape is
-- neutralised by 'message''s existing control-character stripping rather than
-- leaking into the rendered report.
genMsgChar :: Gen Char
genMsgChar = Gen.frequency
  [ (8, Gen.filterT (/= '\n') (Gen.enum ' ' '~'))
  , (1, pure '\ESC')
  , (1, pure '\a')
  ]

genMsg :: Gen Text
genMsg = Gen.text (Range.linear 0 20) genMsgChar

-- | A minimal diagnostic carrier: only 'message' and (optionally) 'url' are
-- ever non-default here, which is all these properties need.
data HD = HD Text (Maybe Url)

instance Diagnostic HD where
  message (HD m _) = pretty m
  url     (HD _ u) = u

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

renderGfx :: HyperlinkMode -> HD -> Text
renderGfx hm d =
  case selectRenderer (withColorMode ColorNever (withUnicodeMode UnicodeAlways
                         (withHyperlinkMode hm (withTarget TGraphical defaultConfig)))) of
    SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r d))
    _                            -> ""

esc :: Char
esc = '\ESC'

-- | Remove OSC 8 escapes (@ESC ]8;; <label> ESC \\@ / @ESC ]8;; ESC \\@),
-- leaving whatever sits between the open and close escapes untouched. Total:
-- a malformed/truncated tail (never produced by 'hyperlink', only reachable if
-- a property here were given bad input) is left as-is rather than crashing.
stripOsc8 :: Text -> Text
stripOsc8 = T.pack . go . T.unpack
  where
    go [] = []
    go (c : ']' : '8' : ';' : ';' : rest)
      | c == esc = case break (== esc) rest of
          (_, _ : '\\' : rest') -> go rest'
          _                     -> rest
    go (c : rest) = c : go rest

-- === pure resolution proofs (mirrors "TermColor"'s colour proofs) =========

prop_explicitPassthrough :: Property
prop_explicitPassthrough = property $ do
  caps <- forAll genCaps
  resolveHyperlink caps HyperlinkAlways === HyperlinkAlways
  resolveHyperlink caps HyperlinkNever  === HyperlinkNever

prop_noHyperlinkWins :: Property
prop_noHyperlinkWins = property $ do
  caps <- forAll (fmap (\c -> c { capNoHyperlink = True }) genCaps)
  resolveHyperlink caps HyperlinkAuto === HyperlinkNever

prop_forceHyperlink :: Property
prop_forceHyperlink = property $ do
  caps <- forAll (fmap (\c -> c { capNoHyperlink = False, capForceHyperlink = True }) genCaps)
  resolveHyperlink caps HyperlinkAuto === HyperlinkAlways

prop_ttyHyperlink :: Property
prop_ttyHyperlink = property $ do
  caps0 <- forAll genCaps
  let caps = caps0 { capNoHyperlink = False, capForceHyperlink = False }
  resolveHyperlink caps HyperlinkAuto === (if capIsTerminal caps then HyperlinkAlways else HyperlinkNever)

prop_noAutoAfter :: Property
prop_noAutoAfter = property $ do
  caps <- forAll genCaps
  hm   <- forAll genHyperlinkMode
  let r = resolveConfig caps (withHyperlinkMode hm defaultConfig)
  assert (configHyperlinkMode r `elem` [HyperlinkAlways, HyperlinkNever])

prop_idempotentHyperlink :: Property
prop_idempotentHyperlink = property $ do
  caps <- forAll genCaps
  hm   <- forAll genHyperlinkMode
  let c1 = resolveConfig caps (withHyperlinkMode hm defaultConfig)
      c2 = resolveConfig caps c1
  configHyperlinkMode c2 === configHyperlinkMode c1

-- === render proofs ==========================================================

prop_neverNoEsc :: Property
prop_neverNoEsc = property $ do
  msg <- forAll genMsg
  mu  <- forAll (Gen.maybe genUrl)
  assert (not (T.any (== esc) (renderGfx HyperlinkNever (HD msg mu))))

prop_alwaysWrapsUrl :: Property
prop_alwaysWrapsUrl = property $ do
  msg <- forAll genMsg
  u   <- forAll genUrl
  let out    = renderGfx HyperlinkAlways (HD msg (Just u))
      label  = unUrl u
      wanted = "\ESC]8;;" <> label <> "\ESC\\" <> label <> "\ESC]8;;\ESC\\"
  assert (T.isInfixOf wanted out)

prop_noUrlNoDifference :: Property
prop_noUrlNoDifference = property $ do
  msg <- forAll genMsg
  renderGfx HyperlinkAlways (HD msg Nothing) === renderGfx HyperlinkNever (HD msg Nothing)

prop_structureInvariant :: Property
prop_structureInvariant = property $ do
  msg <- forAll genMsg
  u   <- forAll genUrl
  stripOsc8 (renderGfx HyperlinkAlways (HD msg (Just u))) === renderGfx HyperlinkNever (HD msg (Just u))

-- 'hyperlink' (like 'colorize') treats any mode that is not the "off" mode as
-- "on"; a raw, unresolved 'HyperlinkAuto' reaching the renderer therefore
-- behaves exactly like 'HyperlinkAlways'. Pinned as a single deterministic
-- case (not a property over generated input) because it documents a specific
-- design choice rather than a general law.
prop_autoUnresolvedWraps :: Property
prop_autoUnresolvedWraps = withTests 1 . property $
  let d = HD "type mismatch" (Just (rightOrErr (mkUrl "https://example.org/errors/E0001")))
  in renderGfx HyperlinkAuto d === renderGfx HyperlinkAlways d

-- A raw ESC embedded in the message text must never reach the rendered
-- report: 'Tadka.Internal.Renderer.Graphical.docToText' already strips
-- control characters from message/help/label text (unrelated to this
-- feature), and this hyperlink work must not weaken that. Checked
-- independently of 'prop_structureInvariant' (which proves the same thing via
-- structural equality): here every ESC in the output is counted directly, so
-- the proof holds even if the two OSC 8 escapes this feature adds were
-- themselves malformed in a way structural comparison could miss.
prop_messageEscNeverLeaks :: Property
prop_messageEscNeverLeaks = property $ do
  msg <- forAll genMsg
  u   <- forAll genUrl
  let out = renderGfx HyperlinkAlways (HD msg (Just u))
  T.count (T.singleton esc) out === 4   -- introducer + terminator, for both the open and close escapes
