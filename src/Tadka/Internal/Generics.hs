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
        Nothing -> case cast v of
          Just (sps :: [Span]) -> ([], [ (T.pack (selName m), sp) | sp <- sps ])
          Nothing               -> ([], [])

instance GCollect U1 where gcollect _ = ([], [])
instance GCollect V1 where gcollect _ = ([], [])

-- === Compile-time shape check ==============================================

type OneSourceManySpans e =
  ( CheckSource (CountField NamedSource (Rep e))
  , CheckSpans  (CountSpanFields (Rep e))
  )

-- Count record fields of a given type in a generic representation. Stays an
-- *exact*-type counter (used only for the NamedSource check): a [Span] field
-- must not accidentally satisfy a Span count, which is exactly the silent-drop
-- bug this fix removes — hence 'CountSpanFields' below is a separate family
-- rather than an overload of this one.
type family CountField (t :: Type) (f :: Type -> Type) :: Nat where
  CountField t (M1 D d f)          = CountField t f
  CountField t (M1 C c f)          = CountField t f
  CountField t (a :*: b)           = CountField t a + CountField t b
  CountField t (M1 S s (K1 R t))   = 1
  CountField t (M1 S s (K1 R c))   = 0
  CountField t U1                  = 0
  CountField t V1                  = 0

-- Count fields that 'gcollect' can turn into span labels: a scalar 'Span' or
-- a '[Span]' field, either one counting as one "this record has spans"
-- field. A record's only span-bearing field may legitimately be a '[Span]'
-- with no scalar 'Span' field at all (e.g. @E { src :: NamedSource, spans ::
-- [Span] }@), which 'gcollect' handles just fine.
type family CountSpanFields (f :: Type -> Type) :: Nat where
  CountSpanFields (M1 D d f)             = CountSpanFields f
  CountSpanFields (M1 C c f)             = CountSpanFields f
  CountSpanFields (a :*: b)              = CountSpanFields a + CountSpanFields b
  CountSpanFields (M1 S s (K1 R Span))   = 1
  CountSpanFields (M1 S s (K1 R [Span])) = 1
  CountSpanFields (M1 S s (K1 R c))      = 0
  CountSpanFields U1                     = 0
  CountSpanFields V1                     = 0

type family CheckSource (n :: Nat) :: Constraint where
  CheckSource 1 = ()
  CheckSource n = TypeError
    ('Text "genericContext: the record must have exactly one NamedSource field, but has "
       ':<>: 'ShowType n)

type family CheckSpans (n :: Nat) :: Constraint where
  CheckSpans 0 = TypeError
    ('Text "genericContext: the record must have at least one Span field")
  CheckSpans n = ()
