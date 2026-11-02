{-# LANGUAGE TemplateHaskell #-}

-- | The @deriveDiagnostic@ Template Haskell splice:
-- an ordinary @Q@-monad splice, validated via 'reify', that generates a
-- 'Diagnostic' instance whose every method body is a direct, unmodified call to
-- a plain function exported from "Tadka.Internal" (or a class default) — so the
-- derive path and a hand-written instance are two doors into the same room.
--
-- The one exception is the default @message@ (@pretty . show@), which has no
-- manual-instance equivalent by definition.
--
-- No compatibility guarantee.
module Tadka.Internal.TH
  ( DiagnosticSpec (..)
  , defaultSpec
    -- * A flatter, declarative alternative to a 'defaultSpec' record update
  , SpecEntry
  , diagnosticSpec
  , diagCode
  , diagSeverity
  , diagHelp
  , diagUrl
  , diagSource
  , diagLabel
  , diagSecondaryLabel
  , diagLabelCollection
  , diagSecondaryLabelCollection
  , diagLabelCollectionWith
  , diagSecondaryLabelCollectionWith
  , diagRelated
  , diagCause
  , diagId
  , diagMessage
  , deriveDiagnostic
  , DiagnosticSumSpec
  , deriveDiagnosticSum
  ) where

import           Control.Monad              (forM)
import           Data.List                  (intercalate, nub, (\\))
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text, pack, unpack)
import           Language.Haskell.TH
import           Prettyprinter              (pretty)

import           Tadka.Internal             (buildContext, buildContextWith,
                                             unsafeDiagnosticCode, unsafeUrl)
import           Tadka.Internal.Context     (Context (NoContext), LabelKind (..))
import           Tadka.Internal.Diagnostic  (Diagnostic (..), SomeDiagnostic)
import           Tadka.Internal.SourceCode  (SomeSource)
import           Tadka.Internal.Span        (Span, SpanF)
import           Tadka.Internal.Types       (DiagnosticId, NamedSource, Severity (..),
                                             mkDiagnosticCode, mkDiagnosticId, mkUrl)

-- | Declarative description of a 'Diagnostic' instance to generate. See
-- 'defaultSpec' for the starting point.
data DiagnosticSpec = DiagnosticSpec
  { specCode        :: Maybe Text        -- ^ validated by 'mkDiagnosticCode' at splice time
  , specSeverity    :: Severity
  , specHelp        :: Maybe Text
  , specUrl         :: Maybe Text         -- ^ validated by 'mkUrl' at splice time
  , specSourceField :: Maybe Name         -- ^ must name a 'NamedSource'- or 'SomeSource'-typed field
  , specLabelFields :: [(Name, Text)]     -- ^ each 'Name' must name a 'Span'-typed field (primary)
  , specSecondaryLabelFields :: [(Name, Text)] -- ^ 'Span'-typed fields rendered as secondary labels
  , specLabelCollectionFields :: [(Name, Text)]
    -- ^ each 'Name' must name a @[Span]@-typed field; every element of that
    -- field's runtime list becomes its own primary label, all sharing the
    -- given text. For a variable number of same-kind occurrences (e.g. every
    -- prior binding of a name) known only at runtime, where 'specLabelFields'
    -- needs one field per label fixed at splice time. Rendered after all
    -- 'specLabelFields' entries, in field order, then list order; an empty
    -- runtime list simply contributes no labels.
  , specSecondaryLabelCollectionFields :: [(Name, Text)]
    -- ^ like 'specLabelCollectionFields', but each element is a secondary
    -- label — the collection counterpart of 'specSecondaryLabelFields'.
  , specLabelCollectionFieldsWith :: [(Name, Q Exp)]
    -- ^ the per-element-text counterpart of 'specLabelCollectionFields':
    -- the 'Name' is carried for documentation and the splice-time
    -- "does this field exist" check only (see 'validateSpecAgainstFields')
    -- — the actual runtime value comes entirely from the paired @Q Exp@,
    -- an @e -> [(Span, Text)]@ expression (quoted with @[| ... |]@)
    -- evaluated once on the record value, already pairing each span with
    -- its own label text. Unlike 'specLabelCollectionFields', which gives
    -- every element in the named field the same literal 'Text', this lets
    -- the text vary per element — e.g. one entry per prior binding of a
    -- name, each labeled with that binding's own description. Because the
    -- shape comes from an arbitrary expression rather than a plain field
    -- reference, 'validateSpecAgainstFields' cannot check its return type
    -- at splice time the way it does for 'specLabelCollectionFields'; a
    -- mismatched type surfaces as an ordinary GHC error at the generated
    -- 'context' method's own call site instead of a friendlier TH 'fail'.
  , specSecondaryLabelCollectionFieldsWith :: [(Name, Q Exp)]
    -- ^ like 'specLabelCollectionFieldsWith', but each element is a
    -- secondary label — the collection counterpart of
    -- 'specSecondaryLabelFields'.
  , specRelated     :: Maybe Name         -- ^ must name a @['SomeDiagnostic']@-typed field
  , specCause       :: Maybe Name         -- ^ must name a @Maybe SomeDiagnostic@-typed field
                                          --   (diagnosticCause's own return type — an exact
                                          --   match, so the generated method is a bare field
                                          --   accessor, same discipline as specRelated/relatedMethod)
  , specId          :: Maybe Name         -- ^ must name a 'Text'- or 'DiagnosticId'-typed field
  , specMessage     :: Maybe (Q Exp)      -- ^ an @e -> Doc Ann@ expression; else @pretty . show@
  }

-- | Everything absent, severity 'SevError'.
defaultSpec :: DiagnosticSpec
defaultSpec = DiagnosticSpec
  { specCode = Nothing, specSeverity = SevError, specHelp = Nothing, specUrl = Nothing
  , specSourceField = Nothing, specLabelFields = [], specSecondaryLabelFields = []
  , specLabelCollectionFields = [], specSecondaryLabelCollectionFields = []
  , specLabelCollectionFieldsWith = [], specSecondaryLabelCollectionFieldsWith = []
  , specRelated = Nothing, specCause = Nothing
  , specId = Nothing, specMessage = Nothing
  }

-- === A flatter, declarative builder, as a lower-risk alternative to a =====
-- === hand-rolled QuasiQuoter ===============================================
--
-- The review this fix answers suggested "a QuasiQuoter closer to miette's
-- @#[diagnostic(...)]@ block" as one way to narrow the gap between tadka's
-- authoring experience and Rust's co-located attributes. A hand-written TH
-- QuasiQuoter is a custom parser with no compiler to check it against in
-- this environment, so it was deliberately not attempted here (see the
-- project's own "do not hallucinate" discipline) — the risk of a subtly
-- wrong parser is strictly higher than a builder made of plain, ordinarily
-- typechecked functions. 'diagnosticSpec' and the 'diag*' combinators below
-- are exactly that: no custom parsing, just a flatter, list-of-directives
-- surface over the same 'DiagnosticSpec' record, so the one-field-at-a-time
-- authoring experience reads closer to a co-located attribute list without
-- introducing any new syntax GHC itself doesn't already parse.

-- | One directive in a 'diagnosticSpec' list: a single field of
-- 'DiagnosticSpec' to set (scalar fields) or append to (the label/collection
-- lists). Opaque — constructed only by the 'diag*' combinators below, so a
-- 'SpecEntry' is always one of exactly the directives this module defines.
newtype SpecEntry = SpecEntry (DiagnosticSpec -> DiagnosticSpec)

-- | Build a 'DiagnosticSpec' from a flat list of directives, applied in
-- order starting from 'defaultSpec'. For a scalar field (code, severity,
-- help, url, source, related, cause, id, message) a later directive
-- overwrites an earlier one, exactly like writing the same field twice in a
-- record update; for a label/collection field, each directive /appends/ one
-- entry, so @['diagLabel' \'a "x", 'diagLabel' \'b "y"]@ means the same as
-- @'specLabelFields' = [(\'a, "x"), (\'b, "y")]@, not the last one winning.
diagnosticSpec :: [SpecEntry] -> DiagnosticSpec
diagnosticSpec = foldl (\s (SpecEntry f) -> f s) defaultSpec

diagCode :: Text -> SpecEntry
diagCode t = SpecEntry (\s -> s { specCode = Just t })

diagSeverity :: Severity -> SpecEntry
diagSeverity sev = SpecEntry (\s -> s { specSeverity = sev })

diagHelp :: Text -> SpecEntry
diagHelp t = SpecEntry (\s -> s { specHelp = Just t })

diagUrl :: Text -> SpecEntry
diagUrl t = SpecEntry (\s -> s { specUrl = Just t })

-- | Set 'specSourceField'. Required before any 'diagLabel'/
-- 'diagSecondaryLabel'/'diagLabelCollection'/'diagSecondaryLabelCollection'/
-- 'diagLabelCollectionWith'/'diagSecondaryLabelCollectionWith' directive, just
-- as 'specSourceField' is required alongside those spec fields directly (see
-- 'requireSourceFieldForLabels').
diagSource :: Name -> SpecEntry
diagSource n = SpecEntry (\s -> s { specSourceField = Just n })

diagLabel :: Name -> Text -> SpecEntry
diagLabel n t = SpecEntry (\s -> s { specLabelFields = specLabelFields s ++ [(n, t)] })

diagSecondaryLabel :: Name -> Text -> SpecEntry
diagSecondaryLabel n t =
  SpecEntry (\s -> s { specSecondaryLabelFields = specSecondaryLabelFields s ++ [(n, t)] })

diagLabelCollection :: Name -> Text -> SpecEntry
diagLabelCollection n t =
  SpecEntry (\s -> s { specLabelCollectionFields = specLabelCollectionFields s ++ [(n, t)] })

diagSecondaryLabelCollection :: Name -> Text -> SpecEntry
diagSecondaryLabelCollection n t = SpecEntry
  (\s -> s { specSecondaryLabelCollectionFields = specSecondaryLabelCollectionFields s ++ [(n, t)] })

-- | See 'specLabelCollectionFieldsWith'.
diagLabelCollectionWith :: Name -> Q Exp -> SpecEntry
diagLabelCollectionWith n qe = SpecEntry
  (\s -> s { specLabelCollectionFieldsWith = specLabelCollectionFieldsWith s ++ [(n, qe)] })

-- | See 'specSecondaryLabelCollectionFieldsWith'.
diagSecondaryLabelCollectionWith :: Name -> Q Exp -> SpecEntry
diagSecondaryLabelCollectionWith n qe = SpecEntry
  (\s -> s { specSecondaryLabelCollectionFieldsWith =
               specSecondaryLabelCollectionFieldsWith s ++ [(n, qe)] })

diagRelated :: Name -> SpecEntry
diagRelated n = SpecEntry (\s -> s { specRelated = Just n })

diagCause :: Name -> SpecEntry
diagCause n = SpecEntry (\s -> s { specCause = Just n })

diagId :: Name -> SpecEntry
diagId n = SpecEntry (\s -> s { specId = Just n })

-- | Set 'specMessage'. Without this directive the generated @message@ falls
-- back to @pretty . show@, exactly as with 'defaultSpec' directly.
diagMessage :: Q Exp -> SpecEntry
diagMessage qe = SpecEntry (\s -> s { specMessage = Just qe })

-- | Field kind for the @diagnosticId@ generator.
data IdKind = IdText | IdDiag

-- | Generate a 'Diagnostic' instance for the named record type.
deriveDiagnostic :: DiagnosticSpec -> Name -> Q [Dec]
deriveDiagnostic spec tyName = do
  fields <- reifyRecordFields tyName
  idInfo <- validateSpecAgainstFields tyName fields spec

  methods <- fmap catMaybes . sequence $
    [ Just <$> messageMethod spec
    , codeMethod (specCode spec)
    , Just <$> severityMethod (specSeverity spec)
    , helpMethod (specHelp spec)
    , urlMethod (specUrl spec)
    , contextMethod (specSourceField spec) (specLabelFields spec) (specSecondaryLabelFields spec)
                    (specLabelCollectionFields spec) (specSecondaryLabelCollectionFields spec)
                    (specLabelCollectionFieldsWith spec) (specSecondaryLabelCollectionFieldsWith spec)
    , relatedMethod (specRelated spec)
    , causeMethod (specCause spec)
    , diagIdMethod idInfo
    ]

  inst <- instanceD (pure []) [t| Diagnostic $(conT tyName) |] (map pure methods)
  pure [inst]

-- | Every field-reference/literal/precondition check a 'DiagnosticSpec'
-- needs, run against one constructor's field list. Shared between
-- 'deriveDiagnostic' (one constructor) and 'deriveDiagnosticSum' (one call
-- per constructor in the sum) so the two entry points can never validate
-- differently -- pulled out unchanged from what was previously
-- 'deriveDiagnostic''s own body.
validateSpecAgainstFields :: Name -> [(Name, Type)] -> DiagnosticSpec -> Q (Maybe (Name, IdKind))
validateSpecAgainstFields tyName fields spec = do
  -- Validate field references and types (compile errors on mismatch).
  mapM_ (\n -> expectHead fields n [''NamedSource, ''SomeSource] "specSourceField") (specSourceField spec)
  mapM_ (\(n, _) -> expectHead fields n [''Span, ''SpanF] "specLabelFields") (specLabelFields spec)
  mapM_ (\(n, _) -> expectHead fields n [''Span, ''SpanF] "specSecondaryLabelFields") (specSecondaryLabelFields spec)
  mapM_ (\(n, _) -> expectListHead fields n [''Span, ''SpanF] "specLabelCollectionFields")
        (specLabelCollectionFields spec)
  mapM_ (\(n, _) -> expectListHead fields n [''Span, ''SpanF] "specSecondaryLabelCollectionFields")
        (specSecondaryLabelCollectionFields spec)
  -- The "With" collection fields carry a 'Name' for documentation and this
  -- existence check only; the actual value comes from the paired 'Q Exp',
  -- not from reading this field, so there is no type shape to check here
  -- (see 'specLabelCollectionFieldsWith').
  mapM_ (\(n, _) -> fieldType fields n >> pure ())
        (specLabelCollectionFieldsWith spec ++ specSecondaryLabelCollectionFieldsWith spec)
  mapM_ (validateRelated fields) (specRelated spec)
  mapM_ (validateCause fields) (specCause spec)
  idInfo <- traverse (\n -> (,) n <$> validateId fields n) (specId spec)

  -- Validate literal code/url at splice time.
  mapM_ (validateLiteral mkDiagnosticCode "specCode") (specCode spec)
  mapM_ (validateLiteral mkUrl            "specUrl")  (specUrl spec)

  -- The default message needs Show.
  requireShowIfDefaultMessage spec tyName

  -- A label with no source field is not a smaller feature, it is a dropped
  -- one: 'contextMethod' can only emit a 'context' method when it has a
  -- source to anchor labels to, so with no 'specSourceField' it emits no
  -- method at all and every label below silently falls back to the class
  -- default ('NoContext') instead of failing loudly. Reject that combination
  -- here, at the one call site that already owns "malformed spec, fail now".
  requireSourceFieldForLabels spec tyName

  pure idInfo

-- === Sum-type (multi-constructor) derivation ==============================

-- | One (constructor name, per-variant spec) entry. A sum-type spec is a
-- list of these, one per constructor of the target type -- checked for
-- completeness (every constructor covered, no unknown constructor named) at
-- splice time in 'deriveDiagnosticSum'.
type DiagnosticSumSpec = [(Name, DiagnosticSpec)]

-- | Sum-type counterpart to 'deriveDiagnostic'. Generates one 'Diagnostic'
-- instance whose every method dispatches on the value's constructor via a
-- single top-level 'case', each arm computed by the same per-field logic
-- 'deriveDiagnostic''s own generators use -- so a sum-type instance and
-- 'deriveDiagnostic' on each variant standing alone produce, per arm,
-- expressions built the identical way (proven in
-- @test/props/Phase13.hs@ the same way Phase 8 proves it for the
-- single-constructor path).
deriveDiagnosticSum :: DiagnosticSumSpec -> Name -> Q [Dec]
deriveDiagnosticSum sumSpec tyName = do
  allCons <- reifyAllConstructors tyName
  requireCompleteSumSpec tyName allCons sumSpec

  enriched <- forM sumSpec $ \(cName, spec) -> do
    fields <- case lookup cName allCons of
      Just fs -> pure fs
      Nothing -> fail (nameBase tyName ++ ": internal error: constructor " ++ nameBase cName
                         ++ " missing from reified constructors after completeness check")
    idInfo <- validateSpecAgainstFields tyName fields spec
    pure (cName, spec, idInfo)

  methods <- sequence
    [ genSumMethod enriched 'message         (\e (_, spec, _)     -> messageMethodArm e spec)
    , genSumMethod enriched 'code            (\_ (_, spec, _)     -> codeMethodArm spec)
    , genSumMethod enriched 'severity        (\_ (_, spec, _)     -> severityMethodArm spec)
    , genSumMethod enriched 'help            (\_ (_, spec, _)     -> helpMethodArm spec)
    , genSumMethod enriched 'url             (\_ (_, spec, _)     -> urlMethodArm spec)
    , genSumMethod enriched 'context         (\e (_, spec, _)     -> contextMethodArm e spec)
    , genSumMethod enriched 'related         (\e (_, spec, _)     -> relatedMethodArm e spec)
    , genSumMethod enriched 'diagnosticCause (\e (_, spec, _)     -> causeMethodArm e spec)
    , genSumMethod enriched 'diagnosticId    (\e (_, _, idInfo)   -> diagIdMethodArm e idInfo)
    ]
  inst <- instanceD (pure []) [t| Diagnostic $(conT tyName) |] (map pure methods)
  pure [inst]

-- | Every constructor of the type must appear exactly once in the sum spec:
-- not missing, not named twice, and not naming a constructor the type
-- doesn't have. A missing constructor would silently produce an incomplete
-- instance rather than a deliberate decision; a duplicated constructor
-- (a plausible copy-paste mistake) is checked and reported before the
-- missing/unknown check below, since the list-difference logic that check
-- uses would otherwise report a duplicated-but-real constructor as
-- "not on this type" -- a correct symptom, but a confusing diagnosis for
-- what is actually a duplicate, not an unknown name; a typo'd constructor
-- name is far easier to debug as a splice-time failure than as a GHC
-- "non-exhaustive case" warning discovered at runtime.
requireCompleteSumSpec :: Name -> [(Name, [(Name, Type)])] -> DiagnosticSumSpec -> Q ()
requireCompleteSumSpec tyName allCons sumSpec = do
  let declaredNames = map fst allCons
      specNames     = map fst sumSpec
      dupes         = nub (specNames \\ nub specNames)
  if not (null dupes)
    then fail (nameBase tyName ++ ": deriveDiagnosticSum spec names constructor(s) more than once: "
                 ++ intercalate ", " (map nameBase dupes))
    else do
      let missing = declaredNames \\ specNames
          unknown = specNames \\ declaredNames
      case (missing, unknown) of
        ([], []) -> pure ()
        (ms, []) -> fail (nameBase tyName ++ ": deriveDiagnosticSum spec is missing constructor(s): "
                            ++ intercalate ", " (map nameBase ms))
        ([], us) -> fail (nameBase tyName ++ ": deriveDiagnosticSum spec names constructor(s) "
                            ++ "not on this type: " ++ intercalate ", " (map nameBase us))
        (ms, us) -> fail (nameBase tyName ++ ": deriveDiagnosticSum spec both is missing "
                            ++ intercalate ", " (map nameBase ms) ++ " and names unknown "
                            ++ intercalate ", " (map nameBase us))

-- | Like 'reifyRecordFields', but for every constructor of a (possibly
-- multi-constructor) 'data' declaration. Each result pairs a constructor's
-- 'Name' with its record field list, in declaration order. Still requires
-- record syntax on every constructor -- mixing record and positional
-- constructors in one sum type is not supported, since 'DiagnosticSpec's
-- field references are name-based.
reifyAllConstructors :: Name -> Q [(Name, [(Name, Type)])]
reifyAllConstructors tyName = do
  info <- reify tyName
  cons <- case info of
    TyConI (DataD _ _ _ _ cs _)   -> pure cs
    TyConI (NewtypeD _ _ _ _ c _) -> pure [c]
    _ -> fail (nameBase tyName ++ ": deriveDiagnosticSum expects a data or newtype declaration")
  traverse fieldsOf cons
  where
    fieldsOf (RecC cName vbts) = pure (cName, [(n, t) | (n, _, t) <- vbts])
    fieldsOf con = fail (nameBase tyName ++ ": deriveDiagnosticSum needs record syntax "
                           ++ "on every constructor (constructor " ++ conNameOf con
                           ++ " is not a record)")
    conNameOf (NormalC n _)     = nameBase n
    conNameOf (RecC n _)        = nameBase n
    conNameOf (InfixC _ n _)    = nameBase n
    conNameOf (ForallC _ _ c)   = conNameOf c
    conNameOf (GadtC ns _ _)    = intercalate "/" (map nameBase ns)
    conNameOf (RecGadtC ns _ _) = intercalate "/" (map nameBase ns)

-- | Build one instance method as a single top-level 'case' over the value's
-- constructor, each arm's RHS produced by @mkArm@ from that constructor's
-- own '(Name, DiagnosticSpec, Maybe (Name, IdKind))' entry. @mkArm@ receives
-- the SAME bound variable ('Name') the outer 'clause' binds, so every arm's
-- body references the one argument the whole method was called with, not a
-- variable local to that arm.
--
-- A constructor whose spec doesn't set a given optional field (help, url,
-- code, related, cause, id) still gets a real arm returning the class
-- default expression (@Nothing@, @[]@, @NoContext@) -- every method is
-- generated for every sum-type instance, never omitted, which keeps this
-- function's shape uniform across all nine methods at the cost of a few
-- bytes of always-@Nothing@ instance code for a spec that never uses a
-- given optional field on any of its constructors.
genSumMethod
  :: [(Name, DiagnosticSpec, Maybe (Name, IdKind))]
  -> Name
  -> (Name -> (Name, DiagnosticSpec, Maybe (Name, IdKind)) -> Q Exp)
  -> Q Dec
genSumMethod enriched methodName mkArm = do
  e <- newName "e"
  arms <- forM enriched $ \entry@(cName, _, _) -> do
    rhs <- mkArm e entry
    pure (match (recP cName []) (normalB (pure rhs)) [])
  funD methodName [clause [varP e] (normalB (caseE (varE e) arms)) []]

-- === Reflection helpers ===================================================

reifyRecordFields :: Name -> Q [(Name, Type)]
reifyRecordFields tyName = do
  info <- reify tyName
  con <- case info of
    TyConI (DataD _ _ _ _ [c] _)  -> pure c
    TyConI (NewtypeD _ _ _ _ c _) -> pure c
    TyConI DataD{}                -> fail (nameBase tyName ++ ": deriveDiagnostic needs a single-constructor record")
    _                             -> fail (nameBase tyName ++ ": deriveDiagnostic expects a data or newtype declaration")
  case con of
    RecC _ vbts -> pure [(n, t) | (n, _, t) <- vbts]
    _           -> fail (nameBase tyName ++ ": deriveDiagnostic needs record syntax with named fields")

fieldType :: [(Name, Type)] -> Name -> Q Type
fieldType fields n = case lookup n fields of
  Just t  -> pure t
  Nothing -> fail ("deriveDiagnostic: " ++ nameBase n ++ " is not a field of the target type")

-- Head 'Name' of a type application, peeling arguments and wrappers.
headName :: Type -> Maybe Name
headName (ConT n)    = Just n
headName (AppT t _)  = headName t
headName (SigT t _)  = headName t
headName (ParensT t) = headName t
headName _           = Nothing

expectHead :: [(Name, Type)] -> Name -> [Name] -> String -> Q ()
expectHead fields n allowed ctx = do
  ty <- fieldType fields n
  case headName ty of
    Just h | h `elem` allowed -> pure ()
    _ -> fail (ctx ++ ": field " ++ nameBase n ++ " has type " ++ pprint ty
                 ++ ", but must be one of " ++ show (map nameBase allowed))

-- | Like 'expectHead', but for a @specLabelCollectionFields@/
-- @specSecondaryLabelCollectionFields@ entry: the field must be a /list/ of
-- one of the allowed heads (@[Span]@, not @Span@).
expectListHead :: [(Name, Type)] -> Name -> [Name] -> String -> Q ()
expectListHead fields n allowed ctx = do
  ty <- fieldType fields n
  case ty of
    AppT ListT inner
      | Just h <- headName inner, h `elem` allowed -> pure ()
    _ -> fail (ctx ++ ": field " ++ nameBase n ++ " has type " ++ pprint ty
                 ++ ", but must be a list of one of " ++ show (map nameBase allowed))

validateRelated :: [(Name, Type)] -> Name -> Q ()
validateRelated fields n = do
  ty <- fieldType fields n
  case ty of
    AppT ListT inner | headName inner == Just ''SomeDiagnostic -> pure ()
    _ -> fail ("specRelated: field " ++ nameBase n ++ " has type " ++ pprint ty
                 ++ ", but must be [SomeDiagnostic]")

-- | Validate that a @specCause@ field is exactly @Maybe SomeDiagnostic@ —
-- 'diagnosticCause''s own return type — so 'causeMethod''s body can be a bare
-- accessor with no wrapping, preserving TH.hs's "every generated method body
-- is an unmodified call" invariant.
validateCause :: [(Name, Type)] -> Name -> Q ()
validateCause fields n = do
  ty <- fieldType fields n
  case ty of
    AppT (ConT m) inner | m == ''Maybe, headName inner == Just ''SomeDiagnostic -> pure ()
    _ -> fail ("specCause: field " ++ nameBase n ++ " has type " ++ pprint ty
                 ++ ", but must be Maybe SomeDiagnostic")

validateId :: [(Name, Type)] -> Name -> Q IdKind
validateId fields n = do
  ty <- fieldType fields n
  case headName ty of
    Just h | h == ''DiagnosticId -> pure IdDiag
           | h == ''Text         -> pure IdText
    _ -> fail ("specId: field " ++ nameBase n ++ " has type " ++ pprint ty
                 ++ ", but must be Text or DiagnosticId")

validateLiteral :: (Text -> Either e a) -> String -> Text -> Q ()
validateLiteral mk ctx t = case mk t of
  Right _ -> pure ()
  Left _  -> fail (ctx ++ ": invalid literal " ++ show (unpack t))

requireShowIfDefaultMessage :: DiagnosticSpec -> Name -> Q ()
requireShowIfDefaultMessage spec tyName = case specMessage spec of
  Just _  -> pure ()
  Nothing -> do
    ok <- isInstance ''Show [ConT tyName]
    if ok then pure ()
          else fail (nameBase tyName ++ ": the default message needs a Show instance"
                       ++ " (add `deriving Show`, or set specMessage)")

-- | 'contextMethod' below only produces a @context@ method when it is given
-- a 'specSourceField'; with none, it produces nothing at all, and the
-- generated instance falls back to the 'Diagnostic' class default
-- (@context _ = NoContext@) regardless of how many label fields the spec
-- names. That fallback is correct for a spec with no labels at all, and
-- wrong for one with labels and no source: every label reference the author
-- wrote would be compiled, accepted, and then never consulted. So this is
-- the one precondition 'contextMethod' cannot check for itself (by the time
-- it pattern-matches on @Nothing@, the label lists are already out of
-- scope) and must instead be enforced here, alongside the spec's other
-- "fail now or silently misbehave later" checks.
requireSourceFieldForLabels :: DiagnosticSpec -> Name -> Q ()
requireSourceFieldForLabels spec tyName = case specSourceField spec of
  Just _  -> pure ()
  Nothing
    | null allLabelFieldNames -> pure ()
    | otherwise -> fail
        (nameBase tyName ++ ": specLabelFields/specSecondaryLabelFields/"
          ++ "specLabelCollectionFields/specSecondaryLabelCollectionFields "
          ++ "name a field (" ++ intercalate ", " (map nameBase allLabelFieldNames)
          ++ ") but specSourceField is Nothing, so no `context` method would "
          ++ "be generated at all and every one of those labels would be "
          ++ "silently dropped. Set specSourceField to the record's "
          ++ "NamedSource (or SomeSource) field, or remove the label fields.")
  where
    allLabelFieldNames =
         map fst (specLabelFields spec)
      ++ map fst (specSecondaryLabelFields spec)
      ++ map fst (specLabelCollectionFields spec)
      ++ map fst (specSecondaryLabelCollectionFields spec)
      ++ map fst (specLabelCollectionFieldsWith spec)
      ++ map fst (specSecondaryLabelCollectionFieldsWith spec)

-- === Method generators (each body is a direct call to a shared function) ===

messageMethod :: DiagnosticSpec -> Q Dec
messageMethod spec = case specMessage spec of
  Just qe -> funD 'message [clause [] (normalB qe) []]
  Nothing -> funD 'message [clause [] (normalB [| pretty . show |]) []]

codeMethod :: Maybe Text -> Q (Maybe Dec)
codeMethod Nothing  = pure Nothing
codeMethod (Just t) = Just <$>
  funD 'code [clause [wildP]
    (normalB [| Just (unsafeDiagnosticCode (pack $(strLit t))) |]) []]

severityMethod :: Severity -> Q Dec
severityMethod sev = funD 'severity [clause [wildP] (normalB (conE (sevCon sev))) []]
  where
    sevCon SevError   = 'SevError
    sevCon SevWarning = 'SevWarning
    sevCon SevAdvice  = 'SevAdvice

helpMethod :: Maybe Text -> Q (Maybe Dec)
helpMethod Nothing  = pure Nothing
helpMethod (Just t) = Just <$>
  funD 'help [clause [wildP] (normalB [| Just (pretty (pack $(strLit t))) |]) []]

urlMethod :: Maybe Text -> Q (Maybe Dec)
urlMethod Nothing  = pure Nothing
urlMethod (Just t) = Just <$>
  funD 'url [clause [wildP] (normalB [| Just (unsafeUrl (pack $(strLit t))) |]) []]

contextMethod :: Maybe Name -> [(Name, Text)] -> [(Name, Text)] -> [(Name, Text)] -> [(Name, Text)]
              -> [(Name, Q Exp)] -> [(Name, Q Exp)] -> Q (Maybe Dec)
contextMethod Nothing _ _ _ _ _ _ = pure Nothing
contextMethod (Just srcN) primFields secFields primColl secColl primCollWith secCollWith = do
  e <- newName "e"
  -- Body is a single direct call to a shared function: buildContext when every
  -- label (fixed-field, literal collection, or per-element-text "with"
  -- collection) is primary, buildContextWith otherwise. A collection field's
  -- list is expanded to one tuple per element, at the same shape
  -- buildContext/buildContextWith already accept, then `concat`ed in after
  -- the fixed-field tuples — buildContext/buildContextWith need no change at
  -- all to accept however many that expansion produces at runtime, including
  -- zero. A "with" collection's list is already @[(Span, Text)]@ (from its
  -- own @Q Exp@, not a field access), so its per-element expansion differs
  -- only in where the text comes from, not in the shape it produces.
  --
  -- `null primColl`/`null secColl`/etc. are decided here at splice time
  -- (these are plain lists in the 'DiagnosticSpec' value, not runtime record
  -- fields), so a spec with no collection fields at all generates the exact
  -- same code this function produced before collection labels existed — not
  -- merely equivalent code with a harmless no-op tail appended.
  body <- if null secFields && null secColl && null secCollWith
            then do
              let single (lf, txt) = [| ($(varE lf) $(varE e), Just (pretty (pack $(strLit txt)))) |]
                  fixedList        = listE (map single primFields)
                  coll (lf, txt)   = [| [ (s, Just (pretty (pack $(strLit txt)))) | s <- $(varE lf) $(varE e) ] |]
                  collWith (_, qe) = [| [ (s, Just (pretty t)) | (s, t) <- $(qe) $(varE e) ] |]
                  extraChunks      = map coll primColl ++ map collWith primCollWith
              if null extraChunks
                then [| buildContext ($(varE srcN) $(varE e)) $(fixedList) |]
                else [| buildContext ($(varE srcN) $(varE e))
                          ($(fixedList) ++ concat $(listE extraChunks)) |]
            else do
              let single k (lf, txt) = [| ($(varE lf) $(varE e), $(k), Just (pretty (pack $(strLit txt)))) |]
                  fixedEntries       = listE (map (single [| Primary |]) primFields
                                            ++ map (single [| Secondary |]) secFields)
                  coll k (lf, txt)   = [| [ (s, $(k), Just (pretty (pack $(strLit txt)))) | s <- $(varE lf) $(varE e) ] |]
                  collWith k (_, qe) = [| [ (s, $(k), Just (pretty t)) | (s, t) <- $(qe) $(varE e) ] |]
                  extraChunks        = map (coll [| Primary |]) primColl
                                     ++ map (coll [| Secondary |]) secColl
                                     ++ map (collWith [| Primary |]) primCollWith
                                     ++ map (collWith [| Secondary |]) secCollWith
              if null extraChunks
                then [| buildContextWith ($(varE srcN) $(varE e)) $(fixedEntries) |]
                else [| buildContextWith ($(varE srcN) $(varE e))
                          ($(fixedEntries) ++ concat $(listE extraChunks)) |]
  Just <$> funD 'context [clause [varP e] (normalB (pure body)) []]

relatedMethod :: Maybe Name -> Q (Maybe Dec)
relatedMethod Nothing   = pure Nothing
relatedMethod (Just rn) = do
  e <- newName "e"
  Just <$> funD 'related [clause [varP e] (normalB [| $(varE rn) $(varE e) |]) []]

causeMethod :: Maybe Name -> Q (Maybe Dec)
causeMethod Nothing   = pure Nothing
causeMethod (Just cn) = do
  e <- newName "e"
  Just <$> funD 'diagnosticCause [clause [varP e] (normalB [| $(varE cn) $(varE e) |]) []]

-- | The 'Name' and its validated 'IdKind' travel together as one value, so a
-- field name can never reach this function without a kind decided for it (the
-- two could previously desync as separately-computed 'Maybe's).
diagIdMethod :: Maybe (Name, IdKind) -> Q (Maybe Dec)
diagIdMethod Nothing = pure Nothing
diagIdMethod (Just (idN, kind)) = do
  e <- newName "e"
  let body = case kind of
        IdText -> [| Just (mkDiagnosticId ($(varE idN) $(varE e))) |]
        IdDiag -> [| Just ($(varE idN) $(varE e)) |]
  Just <$> funD 'diagnosticId [clause [varP e] (normalB body) []]

strLit :: Text -> Q Exp
strLit = litE . stringL . unpack

-- === Sum-type arm generators (each mirrors the method generator above it,
-- returning the VALUE for one constructor instead of a whole method Dec) ===

messageMethodArm :: Name -> DiagnosticSpec -> Q Exp
messageMethodArm e spec = case specMessage spec of
  Just qe -> [| $(qe) $(varE e) |]
  Nothing -> [| pretty (show $(varE e)) |]

codeMethodArm :: DiagnosticSpec -> Q Exp
codeMethodArm spec = case specCode spec of
  Nothing -> [| Nothing |]
  Just t  -> [| Just (unsafeDiagnosticCode (pack $(strLit t))) |]

severityMethodArm :: DiagnosticSpec -> Q Exp
severityMethodArm spec = conE (sevCon (specSeverity spec))
  where
    sevCon SevError   = 'SevError
    sevCon SevWarning = 'SevWarning
    sevCon SevAdvice  = 'SevAdvice

helpMethodArm :: DiagnosticSpec -> Q Exp
helpMethodArm spec = case specHelp spec of
  Nothing -> [| Nothing |]
  Just t  -> [| Just (pretty (pack $(strLit t))) |]

urlMethodArm :: DiagnosticSpec -> Q Exp
urlMethodArm spec = case specUrl spec of
  Nothing -> [| Nothing |]
  Just t  -> [| Just (unsafeUrl (pack $(strLit t))) |]

contextMethodArm :: Name -> DiagnosticSpec -> Q Exp
contextMethodArm e spec = case specSourceField spec of
  Nothing -> [| NoContext |]
  Just srcN ->
    let primFields   = specLabelFields spec
        secFields    = specSecondaryLabelFields spec
        primColl     = specLabelCollectionFields spec
        secColl      = specSecondaryLabelCollectionFields spec
        primCollWith = specLabelCollectionFieldsWith spec
        secCollWith  = specSecondaryLabelCollectionFieldsWith spec
    in if null secFields && null secColl && null secCollWith
         then
           let single (lf, txt) = [| ($(varE lf) $(varE e), Just (pretty (pack $(strLit txt)))) |]
               fixedList         = listE (map single primFields)
               coll (lf, txt)    = [| [ (s, Just (pretty (pack $(strLit txt)))) | s <- $(varE lf) $(varE e) ] |]
               collWith (_, qe)  = [| [ (s, Just (pretty t)) | (s, t) <- $(qe) $(varE e) ] |]
               extraChunks       = map coll primColl ++ map collWith primCollWith
           in if null extraChunks
                then [| buildContext ($(varE srcN) $(varE e)) $(fixedList) |]
                else [| buildContext ($(varE srcN) $(varE e))
                          ($(fixedList) ++ concat $(listE extraChunks)) |]
         else
           let single k (lf, txt) = [| ($(varE lf) $(varE e), $(k), Just (pretty (pack $(strLit txt)))) |]
               fixedEntries       = listE (map (single [| Primary |]) primFields
                                         ++ map (single [| Secondary |]) secFields)
               coll k (lf, txt)   = [| [ (s, $(k), Just (pretty (pack $(strLit txt)))) | s <- $(varE lf) $(varE e) ] |]
               collWith k (_, qe) = [| [ (s, $(k), Just (pretty t)) | (s, t) <- $(qe) $(varE e) ] |]
               extraChunks        = map (coll [| Primary |]) primColl
                                  ++ map (coll [| Secondary |]) secColl
                                  ++ map (collWith [| Primary |]) primCollWith
                                  ++ map (collWith [| Secondary |]) secCollWith
           in if null extraChunks
                then [| buildContextWith ($(varE srcN) $(varE e)) $(fixedEntries) |]
                else [| buildContextWith ($(varE srcN) $(varE e))
                          ($(fixedEntries) ++ concat $(listE extraChunks)) |]

relatedMethodArm :: Name -> DiagnosticSpec -> Q Exp
relatedMethodArm e spec = case specRelated spec of
  Nothing -> [| [] |]
  Just rn -> [| $(varE rn) $(varE e) |]

causeMethodArm :: Name -> DiagnosticSpec -> Q Exp
causeMethodArm e spec = case specCause spec of
  Nothing -> [| Nothing |]
  Just cn -> [| $(varE cn) $(varE e) |]

-- | Mirrors 'diagIdMethod': the 'Name' and its validated 'IdKind' travel
-- together, computed once by 'validateSpecAgainstFields' and threaded
-- through 'deriveDiagnosticSum's @enriched@ list rather than re-derived per
-- arm.
diagIdMethodArm :: Name -> Maybe (Name, IdKind) -> Q Exp
diagIdMethodArm _ Nothing = [| Nothing |]
diagIdMethodArm e (Just (idN, kind)) = case kind of
  IdText -> [| Just (mkDiagnosticId ($(varE idN) $(varE e))) |]
  IdDiag -> [| Just ($(varE idN) $(varE e)) |]
