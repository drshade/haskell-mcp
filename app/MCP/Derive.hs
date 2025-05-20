{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utilities for turning a plain algebraic data type that encodes the
--   various prompt variants into a *runtime* list of 'Prompt' values
--   understood by the rest of the application.
--
--   There are two public entry points:
--
--   @derivePrompts ''MyADT@                       – no doc strings, just stub text
--   @derivePromptsWithDocs ''MyADT 'promptDoc@   – look descriptions up in a
--                                                 value‑level 'DocTable'.
--
--   In the second form the *second* parameter is a quoted name referring to a
--   top‑level 'M.Map Name String' that maps constructor and record‑field
--   names to human‑readable descriptions.
--
module MCP.Derive where

import           Data.Char                  (isUpper, toLower)
import qualified Data.Map.Strict            as M
import           Language.Haskell.TH        (Body (NormalB), Con (..), Dec (..),
                                             Exp (..), ExpQ, Info (TyConI),
                                             Lit (StringL), Name, Pat (..), Q,
                                             Quote (newName), Type (..), conE,
                                             listE, litE, mkName, nameBase,
                                             reify, stringL, varE)
import           Language.Haskell.TH.Syntax (Bang, Clause (..), VarBangType)
import           MCP.Types


-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

snakeCase :: String -> String
snakeCase []     = []
snakeCase (c:cs) = toLower c : go cs
  where
    go (x:xs) | isUpper x = '_' : toLower x : go xs
    go (x:xs)            = x : go xs
    go []                = []

-- very naive pretty‑printer – good enough for docs
renderTy :: Type -> String
renderTy = \case
  ConT n        -> nameBase n
  VarT n        -> nameBase n
  AppT l r      -> renderTy l ++ " " ++ renderTy r
  SigT t _      -> renderTy t
  _             -> "<complex>"

-- | Separate a `Maybe t` into (t, True).  Anything else → (ty, False)
stripMaybe :: Type -> (Type, Bool)
stripMaybe (AppT (ConT n) t) | n == ''Maybe = (t, True)
stripMaybe t                                = (t, False)

lookupArg :: String -> [ArgumentInvocation] -> Maybe String
lookupArg key = go
  where
    go []                                        = Nothing
    go (ArgumentInvocation k v : xs) | k == key  = Just v
                                     | otherwise = go xs

derivePrompts :: Name -> Name -> Q Exp
derivePrompts tyName tblName =
  let lookupDoc nm = let s = nameBase nm in [| M.findWithDefault ("<no doc for " ++ s ++ ">") s $(varE tblName) |]
  in derivePrompts' tyName lookupDoc

deriveTools :: Name -> Name -> Q Exp
deriveTools tyName tblName =
  let lookupDoc nm = let s = nameBase nm in [| M.findWithDefault ("<no doc for " ++ s ++ ">") s $(varE tblName) |]
  in deriveTools' tyName lookupDoc

-------------------------------------------------------------------------------
-- Core generator shared by both front‑ends
-------------------------------------------------------------------------------

derivePrompts' :: Name -> (Name -> Q Exp) -> Q Exp
derivePrompts' ty f =
  deriveDefinitions ty f
      'MkPromptArgumentDefinition
      'MkPromptDefinition

deriveTools' :: Name -> (Name -> Q Exp) -> Q Exp
deriveTools' ty f =
  deriveDefinitions ty f
      'MkToolArgumentDefinition
      'MkToolDefinition

-------------------------------------------------------------------------------
-- | Generic generator used by both prompt and tool derivations.
-------------------------------------------------------------------------------

deriveDefinitions
  :: Name               -- ^ ADT to inspect (e.g. ''PromptInstance)
  -> (Name -> ExpQ)     -- ^ doc lookup (returns an ExpQ String)
  -> Name               -- ^ constructor for *argument* definition
  -> Name               -- ^ constructor for *top level* definition
  -> ExpQ               -- ^ resulting splice: @[Definition]@
deriveDefinitions tyName lookupDoc argCon defCon = do
  TyConI (DataD _ _ _ _ cons _) <- reify tyName
  listE (map build cons)             -- build :: Con -> ExpQ
  where
    ------------------------------------------------------------------------
    -- | Build a definition value for one constructor of the ADT.
    ------------------------------------------------------------------------
    build :: Con -> ExpQ               -- *** returns ExpQ ***
    build (RecC conName fields) = do
      descrQ <- lookupDoc conName                  -- ExpQ String
      let argQs :: [ExpQ]
          argQs = map mkField fields               -- one ExpQ per field
      argsListQ <- listE argQs                    -- ExpQ [ArgDef]
      [| $(conE defCon)
           $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrQ)
           $(pure argsListQ) |]

    build (NormalC conName tys) = do
      descrQ <- lookupDoc conName
      let argQs :: [ExpQ]
          argQs = map mkPos (zip [1 :: Int ..] (map snd tys))
      argsListQ <- listE argQs
      [| $(conE defCon)
           $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrQ)
           $(pure argsListQ) |]

    build other = fail ("deriveDefinitions: unsupported constructor " ++ show other)

    ------------------------------------------------------------------------
    -- | Build an argument definition for a *record* field.
    ------------------------------------------------------------------------
    mkField :: VarBangType -> ExpQ     -- ExpQ ArgDef
    mkField (fName, _, fTy) = do
      let (_, optional) = stripMaybe fTy
          reqFlag :: ExpQ
          reqFlag = if optional then [| False |] else [| True |]
      docQ <- lookupDoc fName
      [| $(conE argCon)
           $(litE $ stringL (nameBase fName))
           $(pure docQ)
           $reqFlag |]

    ------------------------------------------------------------------------
    -- | Build an argument definition for a positional constructor field.
    ------------------------------------------------------------------------
    mkPos :: (Int, Type) -> ExpQ        -- ExpQ ArgDef
    mkPos (ix, ty) = do
      let argName = "arg_" ++ show ix
          (_, optional) = stripMaybe ty
          reqFlag :: ExpQ
          reqFlag = if optional then [| False |] else [| True |]
          fName   = mkName argName
      docQ <- lookupDoc fName
      [| $(conE argCon)
           $(litE $ stringL argName)
           $(pure docQ)
           $reqFlag |]

deriveInvokePrompt :: Name -> Q [Dec]
deriveInvokePrompt adtName = do
  TyConI (DataD _ _ _ _ cons _) <- reify adtName

  matchClauses <- mapM mkClause cons
  fallbackClause <- mkFallback
  let clauses = matchClauses ++ [fallbackClause]

  let funName  = mkName "invokePrompt"
      funSig   = SigD funName (AppT (AppT ArrowT (ConT ''PromptInvocation))
                                    (AppT (AppT (ConT ''Either) (ConT ''String))
                                          (ConT adtName)))
      funDef   = FunD funName clauses
  pure [funSig, funDef]
  where
    ----------------------------------------------------------------
    mkClause :: Con -> Q Clause
    ----------------------------------------------------------------
    mkClause (RecC cName fields) = do
      -- pattern-match on PromptInvocation "code_review" args
      let promptId   = LitP (StringL (snakeCase (nameBase cName)))
      argsName <- newName "args"
      bodyExp  <- buildBody cName fields (VarE argsName)
      pure $ Clause [ConP 'PromptInvocation [] [promptId, VarP argsName]]
                    (NormalB bodyExp) []

    mkClause c =
      fail $ "deriveInvokePrompt: only record constructors supported, saw " ++ show c

    ----------------------------------------------------------------
    -- fallback for unknown prompt id
    mkFallback :: Q Clause
    mkFallback = do
      pidVar  <- newName "pid"
      argsVar <- newName "args"
      errExp <- [| Left ("Unable to match " ++ $(varE pidVar) ++ " to any PromptDefinition (args " ++ show $(varE argsVar) ++ ")" ) |]
      pure $ Clause [ConP 'PromptInvocation [] [VarP pidVar, VarP argsVar]] (NormalB errExp) []

    ----------------------------------------------------------------
    buildBody :: Name -> [VarBangType] -> Exp -> Q Exp
    ----------------------------------------------------------------
    buildBody conName fields argsExp = do
      let mkField :: (Name, Bang, Type) -> Q Exp
          mkField (fName, _, fTy) =
            case stripMaybe fTy of
              (_inner, True ) ->  -- optional
                [| Right ( lookupArg $(litE $ stringL (nameBase fName))
                                  $(pure argsExp)) |]
              (_,     False) ->  -- required
                [| maybe (Left ("missing field " ++ $(litE $ stringL (nameBase fName))))
                        Right
                        (lookupArg $(litE $ stringL (nameBase fName)) $(pure argsExp)) |]

      let fieldExps :: [ExpQ]
          fieldExps = map mkField fields
          startExp  = [| Right $(conE conName) |]  :: ExpQ
          chain acc nxt = [| $acc <*> $nxt |]
          combined = foldl' chain startExp fieldExps  -- ExpQ
      combined

-------------------------------------------------------------------------------
-- Generate runtime parser for tools
-------------------------------------------------------------------------------

deriveInvokeTool :: Name -> Q [Dec]
deriveInvokeTool adtName = do
  TyConI (DataD _ _ _ _ cons _) <- reify adtName

  matchClauses   <- mapM mkClause cons
  fallbackClause <- mkFallback
  let clauses = matchClauses ++ [ fallbackClause ]

  let funName  = mkName "invokeTool"
      funSig   = SigD funName (AppT (AppT ArrowT (ConT ''ToolInvocation))
                                   (AppT (AppT (ConT ''Either) (ConT ''String))
                                         (ConT adtName)))
      funDef   = FunD funName clauses
  pure [funSig, funDef]
  where
    ------------------------------------------------------------------
    mkClause :: Con -> Q Clause
    mkClause (RecC cName fields) = do
      let toolId = LitP (StringL (snakeCase (nameBase cName)))
      argsName  <- newName "args"
      bodyExp   <- buildBody cName fields (VarE argsName)
      pure $ Clause [ConP 'ToolInvocation [] [toolId, VarP argsName]]
                     (NormalB bodyExp) []
    mkClause c = fail $ "deriveInvokeTool: only record constructors supported, saw " ++ show c

    ------------------------------------------------------------------
    mkFallback :: Q Clause
    mkFallback = do
      pidVar  <- newName "pid"
      argsVar <- newName "args"
      errExp  <- [| Left ("Unable to match " ++ $(varE pidVar) ++ " to any ToolDefinition (args " ++ show $(varE argsVar) ++ ")") |]
      pure $ Clause [ConP 'ToolInvocation [] [VarP pidVar, VarP argsVar]] (NormalB errExp) []

    ------------------------------------------------------------------
    buildBody :: Name -> [VarBangType] -> Exp -> Q Exp
    buildBody conName fields argsExp = do
      let mkField (fName, _, fTy) =
            case stripMaybe fTy of
              (_, True)  -> [| Right (lookupArg $(litE $ stringL (nameBase fName)) $(pure argsExp)) |]
              (_, False) -> [| maybe (Left ("missing field " ++ $(litE $ stringL (nameBase fName))))
                                   Right
                                   (lookupArg $(litE $ stringL (nameBase fName)) $(pure argsExp)) |]
          fieldExps :: [ExpQ]
          fieldExps = map mkField fields
          startExp  = [| Right $(conE conName) |] :: ExpQ
          chain acc nxt = [| $acc <*> $nxt |]
          combined = foldl' chain startExp fieldExps
      combined
