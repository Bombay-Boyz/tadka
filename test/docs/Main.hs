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

import           Example
import qualified Examples.AdHoc
import qualified Examples.CustomSource
import qualified Examples.Derive
import qualified Examples.Formats
import qualified Examples.Highlight
import qualified Examples.Quickstart

examples :: [Example]
examples =
  [ Examples.Quickstart.example
  , Examples.Derive.example
  , Examples.CustomSource.example
  , Examples.AdHoc.example
  , Examples.Highlight.example
  , Examples.Formats.example
  ]

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
      results <- mapM checkOne examples
      unless (and results) exitFailure
      putStrLn "all documented examples produce their documented output"

    checkOne ex = do
      out <- exRun ex
      let missing = [ frag | frag <- exMustContain ex, not (frag `T.isInfixOf` out) ]
      if null missing
        then putStrLn ("ok   " ++ exName ex) >> pure True
        else do
          putStrLn ("FAIL " ++ exName ex ++ ": output lacks " ++ show missing)
          TIO.putStrLn out
          pure False
