{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Round-trip interop tests: for each supported library, construct a known
-- failure position, convert it, resolve against the same source, and confirm
-- the line/column matches what the library itself reports (attoparsec, which
-- reports no line/column, is checked on consumed-offset instead).
module Main (main) where

import           Control.Monad             (unless)
import           Data.Text                 (Text)
import           Data.Void                 (Void)
import           System.Exit               (exitFailure)

import qualified Data.Attoparsec.Text      as A
import qualified GHC.Data.Strict            as Strict
import           GHC.Data.FastString        (fsLit)
import           GHC.Types.SrcLoc          (SrcSpan (..), mkRealSrcLoc, mkRealSrcSpan,
                                            srcSpanEndCol, srcSpanEndLine, srcSpanStartCol,
                                            srcSpanStartLine)
import qualified Data.List.NonEmpty         as NE
import           Text.Megaparsec           (Parsec, attachSourcePos, bundleErrors,
                                            bundlePosState, runParser)
import           Text.Megaparsec.Char      (char, string)
import           Text.Megaparsec.Error     (errorOffset)
import           Text.Megaparsec.Pos       (sourceColumn, sourceLine, unPos)

import           Tadka
import           Tadka.Interop.Attoparsec  (consumedOffset)
import           Tadka.Interop.GHC         (SrcSpanConvError (..), spanFromSrcSpan)
import           Tadka.Interop.Megaparsec  (spanFromError)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

resolveAt :: NamedSource -> Span -> (Int, Int)
resolveAt ns sp =
  let rs = rightOrErr (resolveSpan ns sp)
      lc = resolvedStart rs
  in (lcLine lc, lcColumn lc)

-- === GHC ===================================================================
ghcCheck :: (String, Bool)
ghcCheck =
  let src = "aaaa\nbbbbbb\ncccc\n"
      ns  = rightOrErr (mkNamedSource "f.hs" src)
      rss = mkRealSrcSpan (mkRealSrcLoc (fsLit "f.hs") 2 3) (mkRealSrcLoc (fsLit "f.hs") 2 6)
      ss  = RealSrcSpan rss Strict.Nothing
      sp  = rightOrErr (spanFromSrcSpan src ss)
      mine = resolveAt ns sp
      ghc  = (srcSpanStartLine rss, srcSpanStartCol rss)
  in ("ghc SrcSpan line/col round-trips", mine == ghc && mine == (2, 3))

-- A span whose start resolves but whose end is out of bounds must name the
-- END position in the error, not fall back to reporting the start.
ghcEndAttributionCheck :: (String, Bool)
ghcEndAttributionCheck =
  let src = "aaaa\nbbbbbb\ncccc\n"
      rss = mkRealSrcSpan (mkRealSrcLoc (fsLit "f.hs") 2 3) (mkRealSrcLoc (fsLit "f.hs") 99 1)
      ss  = RealSrcSpan rss Strict.Nothing
  in ( "ghc SrcSpan out-of-bounds end is attributed to the end, not the start"
     , spanFromSrcSpan src ss == Left (LineColOutOfBounds (srcSpanEndLine rss) (srcSpanEndCol rss))
     )

-- === megaparsec ============================================================
type P = Parsec Void Text

megaCheck :: (String, Bool)
megaCheck =
  let src   = "abc\ndef"
      ns    = rightOrErr (mkNamedSource "f.hs" src)
      p     = string "abc" *> char '\n' *> string "xyz" :: P Text
  in case runParser p "f.hs" src of
       Right _     -> ("megaparsec offset line/col round-trips", False)
       Left bundle ->
         let err        = NE.head (bundleErrors bundle)
             (withPos,_) = attachSourcePos errorOffset (bundleErrors bundle) (bundlePosState bundle)
             sp'         = snd (NE.head withPos)
             mega        = (unPos (sourceLine sp'), unPos (sourceColumn sp'))
             sp          = rightOrErr (spanFromError 0 err)
             mine        = resolveAt ns sp
         in ("megaparsec offset line/col round-trips", mine == mega && mine == (2, 1))

-- === attoparsec ============================================================
attoCheck :: (String, Bool)
attoCheck =
  let src = "hello\nworld"
      ns  = rightOrErr (mkNamedSource "f.hs" src)
      p   = A.string "hello" *> A.char '\n' *> A.string "xxx"
      res = A.feed (A.parse p src) ""
  in case res of
       A.Fail remaining _ _ ->
         let off  = consumedOffset src remaining
             sp   = rightOrErr (mkSpan off 0)
             mine = resolveAt ns sp
         in ("attoparsec consumed offset resolves", off == 6 && mine == (2, 1))
       _ -> ("attoparsec consumed offset resolves", False)

main :: IO ()
main = do
  let checks = [ghcCheck, ghcEndAttributionCheck, megaCheck, attoCheck]
  results <- mapM report checks
  unless (and results) exitFailure
  where
    report (name, ok) = do
      putStrLn ((if ok then "  ok   " else "  FAIL ") <> name)
      pure ok
