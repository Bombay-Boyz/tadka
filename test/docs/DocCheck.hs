{-# LANGUAGE OverloadedStrings #-}

-- | Keeps the documentation honest. The README and the tutorial may only
-- contain
--
-- * Haskell code that is, character for character, part of a file that is
--   compiled by this package's test suites; and
-- * program output that is exactly what the corresponding example prints.
--
-- A Haskell block is a fenced block whose info string is @haskell@. It must
-- appear as a contiguous run of lines in one of 'sourceFiles'. An output
-- block is a fenced block whose info string is @text@; it must be preceded by
-- a marker line @\<!-- output: NAME --\>@, and must equal the output of the
-- example called @NAME@. Blocks in any other language (shell commands, cabal
-- snippets) are not checked.
module DocCheck (checkDocs) where

import           Data.List     (dropWhileEnd)
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO

import           Example       (Example (..))

-- | The documents that are checked.
docFiles :: [FilePath]
docFiles = ["README.md", "TADKA-TUTORIAL.md", "tadka-highlight/README.md"]

-- | The compiled source that documented code must come from.
sourceFiles :: [FilePath]
sourceFiles =
  [ "test/hello/Main.hs"
  , "tadka-highlight/test/Smoke.hs"
  , "test/docs/Example.hs"
  , "test/docs/Examples/Quickstart.hs"
  , "test/docs/Examples/Derive.hs"
  , "test/docs/Examples/Labels.hs"
  , "test/docs/Examples/Chain.hs"
  , "test/docs/Examples/MultiFile.hs"
  , "test/docs/Examples/Family.hs"
  , "test/docs/Examples/CustomSource.hs"
  , "test/docs/Examples/AdHoc.hs"
  , "test/docs/Examples/Highlight.hs"
  , "test/docs/Examples/Formats.hs"
  , "test/docs/Examples/Configure.hs"
  ]

-- | Names that must not appear in documentation: APIs and licences that no
-- longer exist.
staleTerms :: [Text]
staleTerms =
  [ "withHighlightMode", "HighlightAuto", "HighlightLanguage", "HighlightMode"
  , "Mozilla Public License", "MPL-2.0" ]

data Block = Block
  { blockLine   :: Int
  , blockLang   :: Text
  , blockMarker :: Maybe Text
  , blockBody   :: Text
  }

-- | Fenced blocks, with the output marker (if any) that precedes each.
parseBlocks :: [Text] -> [Block]
parseBlocks = go 1 Nothing
  where
    go :: Int -> Maybe Text -> [Text] -> [Block]
    go _ _ [] = []
    go n prev (l : ls)
      | Just info <- T.stripPrefix "```" l
      , not (T.null (T.strip info)) =
          let (body, rest) = break (== "```") ls
              used         = length body + 2
          in Block n (T.strip info) (prev >>= outputMarker) (T.unlines body)
               : go (n + used) Nothing (drop 1 rest)
      | T.null (T.strip l) = go (n + 1) prev ls
      | otherwise          = go (n + 1) (Just l) ls

-- | The example name in a line like @\<!-- output: quickstart --\>@.
outputMarker :: Text -> Maybe Text
outputMarker l = do
  a <- T.stripPrefix "<!-- output:" (T.strip l)
  b <- T.stripSuffix "-->" a
  pure (T.strip b)

-- | Trailing blanks on every line and blank lines at both ends do not matter.
normalise :: Text -> Text
normalise =
  T.intercalate "\n" . dropWhileEnd T.null . dropWhile T.null . map T.stripEnd . T.lines

-- | Check every document against the compiled sources and the examples'
-- real output. Prints each problem; 'True' when there are none.
checkDocs :: [(Example, Text)] -> IO Bool
checkDocs outputs = do
  sources   <- mapM TIO.readFile sourceFiles
  docs      <- mapM TIO.readFile docFiles
  let corpus = T.intercalate "\n" (map (T.intercalate "\n" . map T.stripEnd . T.lines) sources)
      problems = concat
        [ [ file ++ ": " ++ p | p <- documentProblems corpus outputs doc ]
        | (file, doc) <- zip docFiles docs ]
        ++ consistencyProblems sources
  mapM_ putStrLn problems
  if null problems
    then putStrLn ("ok   documentation (" ++ show (length docFiles) ++ " files)") >> pure True
    else pure False

documentProblems :: Text -> [(Example, Text)] -> Text -> [String]
documentProblems corpus outputs doc =
  concatMap blockProblems (parseBlocks (T.lines doc)) ++ staleProblems
  where
    staleProblems =
      [ "mentions removed name " ++ show t | t <- staleTerms, t `T.isInfixOf` doc ]

    blockProblems b
      | blockLang b == "haskell" =
          [ at b ++ "Haskell block is not part of any compiled example: "
              ++ show (firstLine (blockBody b))
          | not (normalise (blockBody b) `T.isInfixOf` corpus) ]
      | blockLang b == "text" = case blockMarker b of
          Nothing ->
            [ at b ++ "a text block needs an '<!-- output: NAME -->' line before it" ]
          Just name -> case [ out | (ex, out) <- outputs, T.pack (exName ex) == name ] of
            []      -> [ at b ++ "no example is named " ++ show name ]
            (out : _)
              | normalise (blockBody b) == normalise out -> []
              | otherwise ->
                  [ at b ++ "does not match the real output of example " ++ show name ]
      | otherwise = []

    at b = "line " ++ show (blockLine b) ++ ": "
    firstLine = T.unpack . T.strip . head' . T.lines
    head' ls = case ls of
      (x : _) -> x
      []      -> ""

-- | The program in test/hello must define its diagnostic exactly as the
-- quick-start example does, so the output documented for one is the output of
-- the other.
consistencyProblems :: [Text] -> [String]
consistencyProblems sources = case sources of
  (hello : _) ->
    case [ s | s <- sources, "module Examples.Quickstart" `T.isInfixOf` s ] of
      (quick : _) ->
        case chunk quick of
          Just c | normalise c `T.isInfixOf` normalise hello -> []
          _ -> [ "test/hello/Main.hs no longer matches the Quickstart example's definitions" ]
      [] -> [ "the Quickstart example source was not found" ]
  [] -> [ "no sources were read" ]
  where
    startMarker = "-- | What went wrong, and where."
    endMarker   = "(mkSpan 20 8)"
    chunk t =
      let (_, fromStart) = T.breakOn startMarker t
          (body, rest)   = T.breakOn endMarker fromStart
      in if T.null fromStart || T.null rest then Nothing else Just (body <> endMarker)
