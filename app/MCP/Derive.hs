{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MCP.Derive where

import           Data.Char                  (isUpper, toLower)
import qualified Data.Map.Strict            as M
import           Language.Haskell.TH        (Bang (..), Body (NormalB),
                                             Con (..), Dec (..), Exp (..), ExpQ,
                                             Info (TyConI), Lit (StringL), Name,
                                             Pat (..), Q, Quote (newName),
                                             SourceStrictness (..),
                                             SourceUnpackedness (..), Type (..),
                                             conE, listE, litE, mkName,
                                             nameBase, reify, stringL, varE)
import           Language.Haskell.TH.Syntax (Clause (..), VarBangType)
import           MCP.Types

--------------------------------------------------------------------------------
-- Constants and Configuration
--------------------------------------------------------------------------------

-- | Default documentation message for missing docs
defaultDocMessage :: String -> String
defaultDocMessage name = "<no doc for " ++ name ++ ">"

--------------------------------------------------------------------------------
-- String Utilities (Pure Functions)
--------------------------------------------------------------------------------

-- | Convert a Haskell constructor name to snake_case for MCP protocol
-- Examples: GetAllGames -> get_all_games, MessagePlayers -> message_players
snakeCase :: String -> String
snakeCase []     = []
snakeCase (c:cs) = toLower c : go cs
  where
    go (x:xs) | isUpper x = '_' : toLower x : go xs
    go (x:xs)            = x : go xs
    go []                = []

-- | Convert a constructor name to snake_case
makeSnakeCaseName :: Name -> String
makeSnakeCaseName = snakeCase . nameBase

--------------------------------------------------------------------------------
-- Type Analysis (Pure Functions)
--------------------------------------------------------------------------------

-- | Extract the inner type from Maybe wrapper, returning (innerType, isOptional)
-- Examples: Maybe String -> (String, True), Int -> (Int, False)
stripMaybe :: Type -> (Type, Bool)
stripMaybe (AppT (ConT n) t) | n == ''Maybe = (t, True)
stripMaybe t                                = (t, False)

-- | Render a Type to a human-readable string (for debugging)
renderTy :: Type -> String
renderTy = \case
  ConT n        -> nameBase n
  VarT n        -> nameBase n
  AppT l r      -> renderTy l ++ " " ++ renderTy r
  SigT t _      -> renderTy t
  _             -> "<complex>"

--------------------------------------------------------------------------------
-- Argument Lookup Utilities
--------------------------------------------------------------------------------

-- | Look up an argument value by name
lookupArg :: String -> [ArgumentInvocation] -> Maybe String
lookupArg key = go
  where
    go []                                           = Nothing
    go (MkArgumentInvocation k v : xs) | k == key   = Just v
                                     | otherwise    = go xs

--------------------------------------------------------------------------------
-- Constructor Processing (Simplified - Records Only)
--------------------------------------------------------------------------------

-- | Extract constructors from a data type
getConstructors :: Name -> Q [Con]
getConstructors tyName = do
  TyConI (DataD _ _ _ _ cons _) <- reify tyName
  pure cons

-- | Build a single field argument definition
mkFieldArg :: Name -> (Name -> Q Exp) -> VarBangType -> Q Exp
mkFieldArg argCon lookupDoc (fName, _, fTy) = do
  let (_, optional) = stripMaybe fTy
      reqFlag = if optional then [| False |] else [| True |]
  docQ <- lookupDoc fName
  [| $(conE argCon)
       $(litE $ stringL $ nameBase fName)
       $(pure docQ)
       $reqFlag |]

-- | Build a definition from a single constructor
buildConstructorDef :: Name -> Name -> (Name -> Q Exp) -> Con -> Q Exp
buildConstructorDef argCon defCon lookupDoc = \case
  RecC conName fields -> do
    let snakeName = makeSnakeCaseName conName
    descriptionQ <- lookupDoc conName
    argumentQs <- mapM (mkFieldArg argCon lookupDoc) fields
    [| $(conE defCon)
         $(litE $ stringL snakeName)
         $(pure descriptionQ)
         $(listE $ pure <$> argumentQs) |]

  NormalC conName [] -> do
    -- Nullary constructor (no fields)
    let snakeName = makeSnakeCaseName conName
    descriptionQ <- lookupDoc conName
    [| $(conE defCon)
         $(litE $ stringL snakeName)
         $(pure descriptionQ)
         $(listE []) |]

  other ->
    fail $ "Only record constructors and nullary constructors are supported, got: " ++ show other

--------------------------------------------------------------------------------
-- Core Definition Derivation
--------------------------------------------------------------------------------

-- | Common definition derivation logic for both prompts and tools
deriveDefinitions
  :: Name               -- ^ ADT to inspect (e.g. ''PromptInstance)
  -> (Name -> ExpQ)     -- ^ doc lookup (returns an ExpQ String)
  -> Name               -- ^ constructor for *argument* definition
  -> Name               -- ^ constructor for *top level* definition
  -> ExpQ               -- ^ resulting splice: @[Definition]@
deriveDefinitions tyName lookupDoc argCon defCon = do
  cons <- getConstructors tyName
  definitionQs <- mapM (buildConstructorDef argCon defCon lookupDoc) cons
  listE $ pure <$> definitionQs

-- | Derive prompt definitions with documentation lookup
derivePrompts' :: Name -> (Name -> Q Exp) -> Q Exp
derivePrompts' ty f = deriveDefinitions ty f 'MkPromptArgumentDefinition 'MkPromptDefinition

-- | Derive tool definitions with documentation lookup
deriveTools' :: Name -> (Name -> Q Exp) -> Q Exp
deriveTools' ty f = deriveDefinitions ty f 'MkToolArgumentDefinition 'MkToolDefinition

--------------------------------------------------------------------------------
-- Public API (Backwards Compatibility)
--------------------------------------------------------------------------------

-- | Derive prompt definitions using a documentation table
derivePrompts :: Name -> Name -> Q Exp
derivePrompts tyName tblName =
  let lookupDoc nm = let s = nameBase nm in [| M.findWithDefault (defaultDocMessage s) s $(varE tblName) |]
  in derivePrompts' tyName lookupDoc

-- | Derive tool definitions using a documentation table
deriveTools :: Name -> Name -> Q Exp
deriveTools tyName tblName =
  let lookupDoc nm = let s = nameBase nm in [| M.findWithDefault (defaultDocMessage s) s $(varE tblName) |]
  in deriveTools' tyName lookupDoc

--------------------------------------------------------------------------------
-- Invocation Derivation (Simplified)
--------------------------------------------------------------------------------

-- | Build function signature for invoke function
buildFunctionSignature :: Name -> Name -> Q Dec
buildFunctionSignature adtName invTypeName = do
  let funName = mkName ("invoke" ++ nameBase adtName)
      funType = AppT (AppT ArrowT (ConT invTypeName))
                     (AppT (AppT (ConT ''Either) (ConT ''String)) (ConT adtName))
  pure $ SigD funName funType

-- | Build invocation body for a constructor
buildInvocationBody :: Name -> [VarBangType] -> Exp -> Q Exp
buildInvocationBody conName fields argsExp = do
  let mkField (fName, _, fTy) =
        case stripMaybe fTy of
          (_, True ) -> [| Right (lookupArg $(litE $ stringL $ nameBase fName) $(pure argsExp)) |]
          (_, False) -> [| maybe (Left ("missing field " ++ $(litE $ stringL $ nameBase fName)))
                               Right
                               (lookupArg $(litE $ stringL $ nameBase fName) $(pure argsExp)) |]
      fieldQs = map mkField fields
      startQ = [| Right $(conE conName) |]
      chain acc nxt = [| $acc <*> $nxt |]
  foldl chain startQ fieldQs

-- | Create a pattern matching clause for a constructor (records only)
mkClause :: Name -> Con -> Q Clause
mkClause invConName = \case
  RecC cName fields -> do
    let patTag = LitP (StringL (makeSnakeCaseName cName))
    argsName <- newName "_args"
    bodyExp <- buildInvocationBody cName fields (VarE argsName)
    pure $ Clause [ConP invConName [] [patTag, VarP argsName]] (NormalB bodyExp) []

  NormalC cName [] -> do
    let patTag = LitP (StringL (makeSnakeCaseName cName))
    argsName <- newName "_args"
    bodyExp <- [| Right $(conE cName) |]
    pure $ Clause [ConP invConName [] [patTag, VarP argsName]] (NormalB bodyExp) []

  other ->
    fail $ "Only record constructors and nullary constructors are supported, got: " ++ show other

-- | Create fallback clause for unmatched patterns
mkFallback :: Name -> String -> Q Clause
mkFallback invConName errLabel = do
  pid <- newName "p"
  as <- newName "a"
  err <- [| Left ("Unable to match " ++ $(varE pid) ++ " to any " ++ errLabel ++ " (args " ++ show $(varE as) ++ ")") |]
  pure $ Clause [ConP invConName [] [VarP pid, VarP as]] (NormalB err) []

-- | Build function implementation with pattern matching
buildFunctionImplementation :: Name -> [Con] -> Name -> String -> Q Dec
buildFunctionImplementation adtName cons invConName errLabel = do
  let funName = mkName ("invoke" ++ nameBase adtName)

  matchClauses <- mapM (mkClause invConName) cons
  fallbackClause <- mkFallback invConName errLabel
  let clauses = matchClauses ++ [fallbackClause]

  pure $ FunD funName clauses

-- | Generic derivation for invoke functions
deriveInvokeGeneric
  :: Name   -- ^ ADT to reflect (''PromptInstance or ''ToolInstance)
  -> Name   -- ^ Invocation *type* name (''PromptInvocation)
  -> Name   -- ^ Invocation constructor ('MkPromptInvocation)
  -> String -- ^ Human label for error msg ("PromptDefinition" / "ToolDefinition")
  -> Q [Dec]
deriveInvokeGeneric adtName invTypeName invConName errLabel = do
  cons <- getConstructors adtName
  signature <- buildFunctionSignature adtName invTypeName
  implementation <- buildFunctionImplementation adtName cons invConName errLabel
  pure [signature, implementation]

--------------------------------------------------------------------------------
-- Specialized Derivation Functions
--------------------------------------------------------------------------------

-- | Derive invoke function for prompts
deriveInvokePrompt :: Name -> Q [Dec]
deriveInvokePrompt ty = deriveInvokeGeneric ty ''PromptInvocation 'MkPromptInvocation "PromptDefinition"

-- | Derive invoke function for tools
deriveInvokeTool :: Name -> Q [Dec]
deriveInvokeTool ty = deriveInvokeGeneric ty ''ToolInvocation 'MkToolInvocation "ToolDefinition"
