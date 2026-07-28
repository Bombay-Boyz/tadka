{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Golden test suite: renders each graphical fixture and compares byte-for-byte
-- against a committed expected file. Set GEN_GOLDEN=1 to (re)generate the
-- expected files instead of checking.
module Main (main) where

import qualified Data.Aeson                as A
import qualified Data.Aeson.Key            as K
import qualified Data.Aeson.KeyMap         as KM
import           Control.Monad             (forM, unless)
import           Data.Foldable             (toList)
import           Data.List                 (elemIndex, sortBy)
import           Data.Ord                  (comparing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           GHC.IO.Encoding           (setLocaleEncoding, utf8)
import           Prettyprinter             (LayoutOptions (..), PageWidth (Unbounded),
                                            layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)
import           System.Environment        (lookupEnv)
import           System.Exit               (exitFailure)

import           Fixtures                  (contextFixtures, fixtures, generatedSource, jsonFixtures,
                                            narratableFixtures)
import           Tadka

cfg :: Config
cfg = withColorMode ColorNever
    . withUnicodeMode UnicodeAlways
    . withTarget TGraphical
    $ defaultConfig

renderFix :: SomeDiagnostic -> Text
cfgCtx :: Config
cfgCtx = withContextLines 1 cfg

renderCtx :: SomeDiagnostic -> Text
renderCtx (SomeDiagnostic e) = case selectRenderer cfgCtx of
  SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _                            -> ""

renderFix (SomeDiagnostic e) = case selectRenderer cfg of
  SomeRenderer r@(Graphical _) ->
    renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _ -> "<<not graphical>>"

-- Narratable fixtures render at a low depth limit so the truncation marker fires.
narrCfg :: Config
narrCfg = withRelatedDepthLimit 1 (withTarget TNarratable defaultConfig)

renderNarr :: SomeDiagnostic -> Text
renderNarr (SomeDiagnostic e) = case selectRenderer narrCfg of
  SomeRenderer r@(Narratable _) -> render r e
  _                             -> "<<not narratable>>"

-- JSON fixtures render at depth 1 too, then serialize with a deterministic
-- ordered pretty-printer matching the vision's canonical layout.
jsonCfg :: Config
jsonCfg = withRelatedDepthLimit 1 (withTarget TJson defaultConfig)

renderJs :: SomeDiagnostic -> Text
renderJs (SomeDiagnostic e) = case selectRenderer jsonCfg of
  SomeRenderer r@(Json _) -> prettyJSON (render r e)
  _                       -> "<<not json>>"

-- Deterministic pretty-printer for an 'A.Value': object keys in the canonical
-- field order, all-scalar objects inline, empty arrays inline, 2-space indent.
prettyJSON :: A.Value -> Text
prettyJSON = go 0
  where
    prefOrder =
      [ "code","severity","message","labels","line","column","length","text"
      , "primary","stale","help","url","related","causes","truncated","cycleOmitted" ]
    go :: Int -> A.Value -> Text
    go ind v = case v of
      A.Object o -> renderObj ind o
      A.Array a  -> renderArr ind (toList a)
      _          -> TL.toStrict (TLE.decodeUtf8 (A.encode v))
    renderObj ind o
      | KM.null o  = "{}"
      | inlineable = "{ " <> T.intercalate ", " (map field pairs) <> " }"
      | otherwise  = "{\n" <> T.intercalate ",\n" (map (\p -> pad (ind + 1) <> field p) pairs)
                       <> "\n" <> pad ind <> "}"
      where
        pairs      = sortBy (comparing (rank . fst)) (KM.toList o)
        inlineable = all (isSimple . snd) pairs
        field (k, val) = "\"" <> K.toText k <> "\": " <> go (ind + 1) val
        rank k = maybe (1 :: Int, 0) (\i -> (0, i)) (elemIndex (K.toText k) prefOrder)
    renderArr _ []   = "[]"
    renderArr ind xs = "[\n" <> T.intercalate ",\n" (map (\x -> pad (ind + 1) <> go (ind + 1) x) xs)
                         <> "\n" <> pad ind <> "]"
    isSimple (A.Object o) = KM.null o
    isSimple (A.Array a)  = null (toList a)
    isSimple _            = True
    pad n = T.replicate (2 * n) " "

-- (name, rendered output) across all three handlers.
allFixtures :: [(String, Text)]
allFixtures =
     [ (n, renderFix d)  | (n, d) <- fixtures ]
  ++ [ (n, renderNarr d) | (n, d) <- narratableFixtures ]
  ++ [ (n, renderJs d)   | (n, d) <- jsonFixtures ]
  ++ [ (n, renderCtx d) | (n, d) <- contextFixtures ]
  ++ [ ("generated-parseerror", T.pack generatedSource) ]

fixturePath :: String -> FilePath
fixturePath name = "test/golden/fixtures/" <> name <> ".txt"

main :: IO ()
main = do
  setLocaleEncoding utf8
  mode <- lookupEnv "GEN_GOLDEN"
  case mode of
    Just _  -> do
      mapM_ (\(n, out) -> TIO.writeFile (fixturePath n) out) allFixtures
      putStrLn "[golden] regenerated fixtures"
    Nothing -> do
      results <- forM allFixtures $ \(n, actual) -> do
        expected <- TIO.readFile (fixturePath n)
        if actual == expected
          then putStrLn ("  ok  " <> n) >> pure True
          else do
            putStrLn ("  FAIL " <> n <> "\n--- expected ---\n" <> T.unpack expected
                        <> "\n--- actual ---\n" <> T.unpack actual)
            pure False
      unless (and results) exitFailure
