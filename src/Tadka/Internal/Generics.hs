{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- 'OneSourceManySpans' is a compile-time shape guard: its constraints resolve
-- to () or a TypeError and carry no runtime dictionary, so GHC's
-- redundant-constraints check flags them. The guard is intentional.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | GHC-generics label-wiring (vision §6 "GHC-generics path, scoped
-- precisely", spec Phase 9). This derives __only__ 'context', for a record with
-- exactly one 'NamedSource'-typed field and one or more 'Span'-typed fields,
-- using each span field's record-selector name as its label text — by calling
-- the same 'buildContext' the derive macro and manual instances call.
--
-- It deliberately does __not__ touch @code@, @severity@, @help@, @url@,
-- @message@, or @diagnosticId@: those have no structural source, so a
-- generics-wired instance still needs a small hand-written completion of them
-- (each defaultable per the 'Tadka.Diagnostic' class). This is smaller than
-- "most of the ergonomics" — it is exactly one method. For the fuller ergonomic
-- path, use @deriveDiagnostic@ ("Tadka.Internal.TH").
--
-- The record shape is checked at compile time: a record with zero or several
-- 'NamedSource' fields, or no 'Span' field, is a type error — never a silent
-- guess about which field was meant.
--
-- No compatibility guarantee.
module Tadka.Internal.Generics
  ( genericContext
  , GCollect
  ) where

import           Data.Kind              (Constraint, Type)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Typeable          (Typeable, cast)
import           GHC.Generics
import           GHC.TypeLits           (ErrorMessage (..), Nat, TypeError, type (+))
import           Prettyprinter          (pretty)

import           Tadka.Internal.Context (Context (..), buildContext)
import           Tadka.Internal.Span    (Span)
import           Tadka.Internal.Types   (NamedSource)

-- | Derive 'context' for a record with one 'NamedSource' field and one or more
-- 'Span' fields. Intended as @context = genericContext@ inside an otherwise
-- hand-written 'Tadka.Diagnostic' instance.
genericContext
  :: forall e
   . (Generic e, GCollect (Rep e), OneSourceManySpans e)
  => e -> Context
genericContext e =
  case gcollect (from e) of
    (src : _, labels) -> buildContext src [ (sp, Just (pretty nm)) | (nm, sp) <- labels ]
    ([], _)           -> NoContext   -- unreachable given 'OneSourceManySpans'

-- === Value collection via generics =========================================

-- | Collect the 'NamedSource' fields and the @(label text, 'Span')@ pairs from
-- a generic representation. Field values are classified by 'cast', so only
-- 'NamedSource'- and 'Span'-typed fields contribute; all others are ignored.
class GCollect f where
  gcollect :: f p -> ([NamedSource], [(Text, Span)])

instance GCollect f => GCollect (M1 D d f) where gcollect (M1 x) = gcollect x
instance GCollect f => GCollect (M1 C c f) where gcollect (M1 x) = gcollect x

instance (GCollect a, GCollect b) => GCollect (a :*: b) where
  gcollect (a :*: b) = gcollect a <> gcollect b

instance (Selector s, Typeable c) => GCollect (M1 S s (K1 R c)) where
  gcollect m@(M1 (K1 v)) =
    case cast v of
      Just (src :: NamedSource) -> ([src], [])
      Nothing -> case cast v of
        Just (sp :: Span) -> ([], [(T.pack (selName m), sp)])
        Nothing           -> ([], [])

instance GCollect U1 where gcollect _ = ([], [])
instance GCollect V1 where gcollect _ = ([], [])

-- === Compile-time shape check ==============================================

type OneSourceManySpans e =
  ( CheckSource (CountField NamedSource (Rep e))
  , CheckSpans  (CountField Span        (Rep e))
  )

-- Count record fields of a given type in a generic representation.
type family CountField (t :: Type) (f :: Type -> Type) :: Nat where
  CountField t (M1 D d f)          = CountField t f
  CountField t (M1 C c f)          = CountField t f
  CountField t (a :*: b)           = CountField t a + CountField t b
  CountField t (M1 S s (K1 R t))   = 1
  CountField t (M1 S s (K1 R c))   = 0
  CountField t U1                  = 0
  CountField t V1                  = 0

type family CheckSource (n :: Nat) :: Constraint where
  CheckSource 1 = ()
  CheckSource n = TypeError
    ('Text "genericContext: the record must have exactly one NamedSource field, but has "
       ':<>: 'ShowType n)

type family CheckSpans (n :: Nat) :: Constraint where
  CheckSpans 0 = TypeError
    ('Text "genericContext: the record must have at least one Span field")
  CheckSpans n = ()
