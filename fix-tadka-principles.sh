#!/usr/bin/env bash
# fix-tadka-principles.sh
#
# Applies the three principles.md fixes identified in the code audit:
#   1. interop/ghc/.../GHC.hs   — replace partial `!!` with a total lookup
#   2. src/.../TH.hs            — tie specId's Name to its IdKind as one value
#   3. tools/gen-width-table.hs — replace `error` calls with Either + a single
#                                 IO-edge exit, instead of crashing mid-parse
#
# Run from the tadka project root (the directory containing tadka.cabal).
# Idempotent: if a fix is already applied, that step is skipped, not failed.
# Fails loudly (no silent skips) if a file is missing or its content doesn't
# match what's expected, rather than silently doing nothing.

set -euo pipefail

ROOT="$(pwd)"

if [ ! -f "$ROOT/tadka.cabal" ]; then
  echo "error: tadka.cabal not found in $ROOT — run this from the tadka project root." >&2
  exit 1
fi

GHC_FILE="$ROOT/interop/ghc/Tadka/Interop/GHC.hs"
TH_FILE="$ROOT/src/Tadka/Internal/TH.hs"
GEN_FILE="$ROOT/tools/gen-width-table.hs"

for f in "$GHC_FILE" "$TH_FILE" "$GEN_FILE"; do
  if [ ! -f "$f" ]; then
    echo "error: expected file not found: $f" >&2
    exit 1
  fi
done

python3 - "$GHC_FILE" "$TH_FILE" "$GEN_FILE" <<'PYEOF'
import sys

ghc_file, th_file, gen_file = sys.argv[1:4]

def apply_fix(path, old, new, marker, label):
    with open(path, "r", encoding="utf-8") as fh:
        text = fh.read()
    if marker in text:
        print(f"skip:  {label} (already applied)")
        return
    if old not in text:
        raise SystemExit(
            f"error: {label}: expected original text not found in {path}.\n"
            f"       The file may have changed since this script was written — "
            f"apply this fix by hand instead."
        )
    text = text.replace(old, new, 1)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)
    print(f"fixed: {label}")


# --- Fix 1: GHC.hs — partial !! -> total atMayList --------------------------

ghc_old = '''-- | Convert a 1-based (line, column) into a 0-based character offset within the
-- given source, or 'Nothing' if the position is not in bounds. Column may point
-- one past the end of a line (the end-of-line position GHC uses).
offsetFromLineCol :: Text -> Int -> Int -> Maybe Int
offsetFromLineCol src line col
  | line < 1 || line > length ls = Nothing
  | col  < 1 || col  > T.length here + 1 = Nothing
  | otherwise = Just (before + (col - 1))
  where
    ls     = T.splitOn "\\n" src
    here   = ls !! (line - 1)
    before = sum (map ((+ 1) . T.length) (take (line - 1) ls))  -- +1 per newline'''

ghc_new = '''-- | Total, safe indexing into a list: 'Nothing' out of bounds, never a partial
-- crash. Kept local (mirrors 'Tadka.Internal.Width.atMay') so no caller in this
-- module ever reaches for the partial @(!!)@ directly.
atMayList :: [a] -> Int -> Maybe a
atMayList xs i
  | i < 0     = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      []      -> Nothing

-- | Convert a 1-based (line, column) into a 0-based character offset within the
-- given source, or 'Nothing' if the position is not in bounds. Column may point
-- one past the end of a line (the end-of-line position GHC uses). Total: the
-- line lookup is tied directly to the 'Maybe' via 'atMayList', so an
-- out-of-range line can never reach the column check below it.
offsetFromLineCol :: Text -> Int -> Int -> Maybe Int
offsetFromLineCol src line col = do
  here <- atMayList ls (line - 1)
  if col < 1 || col > T.length here + 1
    then Nothing
    else Just (before + (col - 1))
  where
    ls     = T.splitOn "\\n" src
    before = sum (map ((+ 1) . T.length) (take (line - 1) ls))  -- +1 per newline'''

apply_fix(ghc_file, ghc_old, ghc_new, "atMayList ::", "GHC.hs: partial !! -> total atMayList")


# --- Fix 2: TH.hs — decouple specId Name from its IdKind --------------------

th_old_1 = '''  mapM_ (validateRelated fields) (specRelated spec)
  mIdKind <- traverse (validateId fields) (specId spec)'''
th_new_1 = '''  mapM_ (validateRelated fields) (specRelated spec)
  idInfo <- traverse (\\n -> (,) n <$> validateId fields n) (specId spec)'''

th_old_2 = '''    , diagIdMethod (specId spec) mIdKind
    ]'''
th_new_2 = '''    , diagIdMethod idInfo
    ]'''

th_old_3 = '''diagIdMethod :: Maybe Name -> Maybe IdKind -> Q (Maybe Dec)
diagIdMethod Nothing _ = pure Nothing
diagIdMethod (Just idN) kind = do
  e <- newName "e"
  let body = case kind of
        Just IdText -> [| Just (mkDiagnosticId ($(varE idN) $(varE e))) |]
        _           -> [| Just ($(varE idN) $(varE e)) |]
  Just <$> funD 'diagnosticId [clause [varP e] (normalB body) []]'''
th_new_3 = '''-- | The 'Name' and its validated 'IdKind' travel together as one value, so a
-- field name can never reach this function without a kind decided for it (the
-- two could previously desync as separately-computed 'Maybe's).
diagIdMethod :: Maybe (Name, IdKind) -> Q (Maybe Dec)
diagIdMethod Nothing = pure Nothing
diagIdMethod (Just (idN, kind)) = do
  e <- newName "e"
  let body = case kind of
        IdText -> [| Just (mkDiagnosticId ($(varE idN) $(varE e))) |]
        IdDiag -> [| Just ($(varE idN) $(varE e)) |]
  Just <$> funD 'diagnosticId [clause [varP e] (normalB body) []]'''

with open(th_file, "r", encoding="utf-8") as fh:
    th_text = fh.read()

if "idInfo <-" in th_text:
    print("skip:  TH.hs: decouple specId/IdKind (already applied)")
else:
    for old, new, label in [
        (th_old_1, th_new_1, "call site (traverse)"),
        (th_old_2, th_new_2, "call site (diagIdMethod invocation)"),
        (th_old_3, th_new_3, "diagIdMethod definition"),
    ]:
        if old not in th_text:
            raise SystemExit(
                f"error: TH.hs: expected original text not found for {label}.\n"
                f"       The file may have changed since this script was written — "
                f"apply this fix by hand instead."
            )
        th_text = th_text.replace(old, new, 1)
    with open(th_file, "w", encoding="utf-8") as fh:
        fh.write(th_text)
    print("fixed: TH.hs: decouple specId/IdKind into one correlated value")


# --- Fix 3: gen-width-table.hs — error calls -> Either + IO-edge die --------

with open(gen_file, "r", encoding="utf-8") as fh:
    gen_text = fh.read()

if "System.Exit" in gen_text:
    print("skip:  gen-width-table.hs: error -> Either (already applied)")
else:
    gen_old_import = '''import           System.Directory  (createDirectoryIfMissing, doesFileExist)
import           System.FilePath   ((</>))'''
    gen_new_import = '''import           System.Directory  (createDirectoryIfMissing, doesFileExist)
import           System.Exit       (die)
import           System.FilePath   ((</>))'''

    gen_old_body = '''readUcd :: FilePath -> IO [((Int, Int), Text)]
readUcd name = do
  contents <- TIO.readFile (cacheDir </> name)
  pure (concatMap parseLine (T.lines contents))

-- | Parse one UCD data line into a code-point range and its property value.
-- Comments (@#@ onward) and blank lines yield no result.
parseLine :: Text -> [((Int, Int), Text)]
parseLine raw =
  case T.strip (T.takeWhile (/= '#') raw) of
    body | T.null body -> []
         | otherwise ->
             case map T.strip (T.splitOn ";" body) of
               (codes : prop : _) -> [(parseCodes codes, prop)]
               _                  -> []

parseCodes :: Text -> (Int, Int)
parseCodes t =
  case map hex (T.splitOn ".." t) of
    [x]    -> (x, x)
    [a, b] -> (a, b)
    _      -> error ("gen-width-table: malformed code range: " <> T.unpack t)

hex :: Text -> Int
hex t = case readHex (T.unpack t) of
  [(n, "")] -> n
  _         -> error ("gen-width-table: bad hex: " <> T.unpack t)'''

    gen_new_body = '''-- | Read and parse a cached UCD file, or a descriptive failure. No partial
-- function fires while parsing; a malformed line is reported by value and
-- only turned into a process exit at the IO boundary in 'main'.
readUcd :: FilePath -> IO [((Int, Int), Text)]
readUcd name = do
  contents <- TIO.readFile (cacheDir </> name)
  case concat <$> traverse parseLine (T.lines contents) of
    Right ranges -> pure ranges
    Left err     -> die ("gen-width-table: " <> name <> ": " <> err)

-- | Parse one UCD data line into a code-point range and its property value.
-- Comments (@#@ onward) and blank lines yield no result. Total: failure is
-- reported via 'Left', never a crash.
parseLine :: Text -> Either String [((Int, Int), Text)]
parseLine raw =
  case T.strip (T.takeWhile (/= '#') raw) of
    body | T.null body -> Right []
         | otherwise ->
             case map T.strip (T.splitOn ";" body) of
               (codes : prop : _) -> (\\r -> [(r, prop)]) <$> parseCodes codes
               _                  -> Right []

parseCodes :: Text -> Either String (Int, Int)
parseCodes t =
  case traverse hex (T.splitOn ".." t) of
    Right [x]    -> Right (x, x)
    Right [a, b] -> Right (a, b)
    Right _      -> Left ("malformed code range: " <> T.unpack t)
    Left err     -> Left err

hex :: Text -> Either String Int
hex t = case readHex (T.unpack t) of
  [(n, "")] -> Right n
  _         -> Left ("bad hex: " <> T.unpack t)'''

    if gen_old_import not in gen_text or gen_old_body not in gen_text:
        raise SystemExit(
            "error: gen-width-table.hs: expected original text not found.\n"
            "       The file may have changed since this script was written — "
            "apply this fix by hand instead."
        )
    gen_text = gen_text.replace(gen_old_import, gen_new_import, 1)
    gen_text = gen_text.replace(gen_old_body, gen_new_body, 1)
    with open(gen_file, "w", encoding="utf-8") as fh:
        fh.write(gen_text)
    print("fixed: gen-width-table.hs: error calls -> Either + IO-edge die")

print("")
print("Done. Review the diffs (git diff) before committing, and re-run your")
print("golden/property/interop test suites (make test) to confirm nothing broke.")
PYEOF
