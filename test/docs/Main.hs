{-# LANGUAGE OverloadedStrings #-}

-- | Runs every documented example and checks its output.
--
-- > cabal test docs                      -- check the examples
-- > cabal run test:docs -- --print       -- print their real output
--
-- The documentation quotes the output of @--print@, never output typed by hand.
module Main (main) where

import           Control.Monad       (unless)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           GHC.IO.Encoding     (setLocaleEncoding, utf8)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)
import           System.IO           (hSetEncoding, stdout)

import           DocCheck            (checkDocs)
import           Example
import qualified Examples.AdHoc
import qualified Examples.Chain
import qualified Examples.Configure
import qualified Examples.CustomSource
import qualified Examples.Derive
import qualified Examples.Family
import qualified Examples.Formats
import qualified Examples.Highlight
import qualified Examples.Labels
import qualified Examples.MultiFile
import qualified Examples.Quickstart

examples :: [Example]
examples =
  [ Examples.Quickstart.example ]
  ++ [ Examples.Derive.example ]
  ++ Examples.Labels.examples
  ++ Examples.Chain.examples
  ++ [ Examples.MultiFile.example ]
  ++ Examples.Family.examples
  ++ [ Examples.CustomSource.example ]
  ++ [ Examples.AdHoc.example ]
  ++ [ Examples.Highlight.example ]
  ++ Examples.Formats.examples
  ++ [ Examples.Configure.example ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  args <- getArgs
  if "--print" `elem` args then mapM_ printOne examples else check
  where
    printOne ex = do
      putStrLn ("===== " ++ exName ex ++ " =====")
      out <- exRun ex
      TIO.putStrLn out
      putStrLn ""

    check = do
      outputs <- mapM (\ex -> (,) ex <$> exRun ex) examples
      results <- mapM checkOne outputs
      docsOk  <- checkDocs outputs
      unless (and results && docsOk) exitFailure
      putStrLn "every example runs, and the documentation matches them"

    checkOne (ex, out) = do
      let missing = [ frag | frag <- exMustContain ex, not (frag `T.isInfixOf` out) ]
      if null missing
        then putStrLn ("ok   " ++ exName ex) >> pure True
        else do
          putStrLn ("FAIL " ++ exName ex ++ ": output lacks " ++ show missing)
          TIO.putStrLn out
          pure False
