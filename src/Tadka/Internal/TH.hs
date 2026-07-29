{-# LANGUAGE TemplateHaskell #-}

-- | The @deriveDiagnostic@ Template Haskell splice (vision §6, spec Phase 8):
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
  , deriveDiagnostic
  ) where

import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text, pack, unpack)
import           Language.Haskell.TH
import           Prettyprinter              (pretty)

import           Tadka.Internal             (buildContext, buildContextWith,
                                             unsafeDiagnosticCode, unsafeUrl)
import           Tadka.Internal.Context     (LabelKind (..))
import           Tadka.Internal.Diagnostic  (Diagnostic (..), SomeDiagnostic)
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
  , specSourceField :: Maybe Name         -- ^ must name a 'NamedSource'-typed field
  , specLabelFields :: [(Name, Text)]     -- ^ each 'Name' must name a 'Span'-typed field (primary)
  , specSecondaryLabelFields :: [(Name, Text)] -- ^ 'Span'-typed fields rendered as secondary labels
  , specRelated     :: Maybe Name         -- ^ must name a @['SomeDiagnostic']@-typed field
  , specId          :: Maybe Name         -- ^ must name a 'Text'- or 'DiagnosticId'-typed field
  , specMessage     :: Maybe (Q Exp)      -- ^ an @e -> Doc Ann@ expression; else @pretty . show@
  }

-- | Everything absent, severity 'SevError'.
defaultSpec :: DiagnosticSpec
defaultSpec = DiagnosticSpec
  { specCode = Nothing, specSeverity = SevError, specHelp = Nothing, specUrl = Nothing
  , specSourceField = Nothing, specLabelFields = [], specSecondaryLabelFields = []
  , specRelated = Nothing
  , specId = Nothing, specMessage = Nothing
  }

-- | Field kind for the @diagnosticId@ generator.
data IdKind = IdText | IdDiag

-- | Generate a 'Diagnostic' instance for the named record type.
deriveDiagnostic :: DiagnosticSpec -> Name -> Q [Dec]
deriveDiagnostic spec tyName = do
  fields <- reifyRecordFields tyName

  -- Validate field references and types (compile errors on mismatch).
  mapM_ (\n -> expectHead fields n [''NamedSource] "specSourceField") (specSourceField spec)
  mapM_ (\(n, _) -> expectHead fields n [''Span, ''SpanF] "specLabelFields") (specLabelFields spec)
  mapM_ (\(n, _) -> expectHead fields n [''Span, ''SpanF] "specSecondaryLabelFields") (specSecondaryLabelFields spec)
  mapM_ (validateRelated fields) (specRelated spec)
  idInfo <- traverse (\n -> (,) n <$> validateId fields n) (specId spec)

  -- Validate literal code/url at splice time.
  mapM_ (validateLiteral mkDiagnosticCode "specCode") (specCode spec)
  mapM_ (validateLiteral mkUrl            "specUrl")  (specUrl spec)

  -- The default message needs Show.
  requireShowIfDefaultMessage spec tyName

  methods <- fmap catMaybes . sequence $
    [ Just <$> messageMethod spec
    , codeMethod (specCode spec)
    , Just <$> severityMethod (specSeverity spec)
    , helpMethod (specHelp spec)
    , urlMethod (specUrl spec)
    , contextMethod (specSourceField spec) (specLabelFields spec) (specSecondaryLabelFields spec)
    , relatedMethod (specRelated spec)
    , diagIdMethod idInfo
    ]

  inst <- instanceD (pure []) [t| Diagnostic $(conT tyName) |] (map pure methods)
  pure [inst]

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

validateRelated :: [(Name, Type)] -> Name -> Q ()
validateRelated fields n = do
  ty <- fieldType fields n
  case ty of
    AppT ListT inner | headName inner == Just ''SomeDiagnostic -> pure ()
    _ -> fail ("specRelated: field " ++ nameBase n ++ " has type " ++ pprint ty
                 ++ ", but must be [SomeDiagnostic]")

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

contextMethod :: Maybe Name -> [(Name, Text)] -> [(Name, Text)] -> Q (Maybe Dec)
contextMethod Nothing _ _ = pure Nothing
contextMethod (Just srcN) primFields secFields = do
  e <- newName "e"
  -- Body is a single direct call to a shared function: buildContext when all
  -- labels are primary (matching the vision), buildContextWith otherwise.
  body <- if null secFields
            then do
              let tup (lf, txt) = [| ($(varE lf) $(varE e), Just (pretty (pack $(strLit txt)))) |]
              [| buildContext ($(varE srcN) $(varE e)) $(listE (map tup primFields)) |]
            else do
              let tup k (lf, txt) = [| ($(varE lf) $(varE e), $(k), Just (pretty (pack $(strLit txt)))) |]
                  entries = map (tup [| Primary |]) primFields ++ map (tup [| Secondary |]) secFields
              [| buildContextWith ($(varE srcN) $(varE e)) $(listE entries) |]
  Just <$> funD 'context [clause [varP e] (normalB (pure body)) []]

relatedMethod :: Maybe Name -> Q (Maybe Dec)
relatedMethod Nothing   = pure Nothing
relatedMethod (Just rn) = do
  e <- newName "e"
  Just <$> funD 'related [clause [varP e] (normalB [| $(varE rn) $(varE e) |]) []]

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
