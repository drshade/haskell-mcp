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

import           Data.Aeson                 (ToJSON (..), object, (.=))
import qualified Data.Aeson                 as A
import           Data.Aeson.Key             (fromString)
import           Data.Char                  (isUpper, toLower)
import qualified Data.Map.Strict            as M
import           Language.Haskell.TH        (Body (NormalB), Con (..), Dec (..),
                                             Exp (..), ExpQ, Info (TyConI),
                                             Lit (StringL), Name, Pat (..), Q,
                                             Quote (newName), Type (..), conE,
                                             listE, litE, mkName, nameBase,
                                             reify, stringL, varE)
import           Language.Haskell.TH.Syntax (Bang, Clause (..), VarBangType)


type ArgumentName = String
type ArgumentDescription = String
type ArgumentRequired = Bool

data PromptArgumentDefinition = PromptArgumentDefinition ArgumentName ArgumentDescription ArgumentRequired
  deriving (Show)

data ToolArgumentDefinition = ToolArgumentDefinition ArgumentName ArgumentDescription ArgumentRequired
  deriving (Show)

data PromptDefinition = PromptDefinition String String [PromptArgumentDefinition]
  deriving (Show)

data ToolDefinition = ToolDefinition String String [ToolArgumentDefinition]
  deriving (Show)

data ArgumentInvocation = ArgumentInvocation String String
  deriving (Show)

data PromptInvocation = PromptInvocation String [ArgumentInvocation]
  deriving (Show)

data ToolInvocation = ToolInvocation String [ArgumentInvocation]
  deriving (Show)


-------------------------------------------------------------------------------
-- | Friendly alias for a documentation lookup table.
--   Keys *must* be the Names of constructors or record field labels.
-------------------------------------------------------------------------------

type DocTable = M.Map String String

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

derivePrompts' :: Name            -- ^ datatype to reflect
        -> (Name -> Q Exp) -- ^ doc lookup: key → TH Exp that yields String
        -> Q Exp
derivePrompts' tyName lookupDoc = do
  TyConI (DataD _ _ _ _ cons _) <- reify tyName
  listEs <- mapM (\case
    RecC conName fields          -> genRecord conName fields
    NormalC conName types        -> genPositional conName (map snd types)
    c                            -> fail $ "Unsupported constructor: " ++ show c) cons
  pure (ListE listEs)
  where
    -------------------------------------------------------------------------
    genRecord conName fields = do
      descrE <- lookupDoc conName
      let argEs :: [ExpQ]
          argEs = map mkArg fields
              where
                mkArg (fName, _, fTy) = do
                  let (_innerTy, optional) = stripMaybe fTy
                      promptId            = snakeCase (nameBase fName)
                  docE <- lookupDoc fName
                  let reqFlag = ConE (if optional then 'False else 'True)
                  [| PromptArgumentDefinition $(litE $ stringL promptId)
                                     $(pure docE)
                                     $(pure reqFlag) |]
      argListE <- listE argEs
      [| PromptDefinition $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrE) $(pure argListE) |]

    -------------------------------------------------------------------------
    genPositional conName tys = do
      descrE <- lookupDoc conName
      let argEs :: [ExpQ]
          argEs = map mkArg (zip [1 :: Int ..] tys)
              where
                mkArg (ix, ty) = do
                  let (_innerTy, optional) = stripMaybe ty
                      fStr                = "arg_" ++ show ix
                      fName               = mkName fStr
                  docE <- lookupDoc fName
                  let reqFlag = ConE (if optional then 'False else 'True)
                  [| PromptArgumentDefinition $(litE $ stringL fStr)
                                     $(pure docE)
                                     $(pure reqFlag) |]
      argListE <- listE argEs
      [| PromptDefinition $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrE) $(pure argListE) |]

deriveTools' :: Name -> (Name -> Q Exp) -> Q Exp
deriveTools' tyName lookupDoc = do
  TyConI (DataD _ _ _ _ cons _) <- reify tyName
  listEs <- mapM (\case
    RecC conName fields -> genRecord conName fields
    NormalC conName tys -> genPositional conName (map snd tys)
    c -> fail $ "Unsupported constructor: " ++ show c) cons
  pure (ListE listEs)
  where
    genRecord conName fields = do
      descrE <- lookupDoc conName
      let argEs = map mkArg fields
            where mkArg (fName,_,fTy) = do
                    let (_,opt) = stripMaybe fTy
                    docE <- lookupDoc fName
                    let reqFlag = ConE (if opt then 'False else 'True)
                    [| ToolArgumentDefinition $(litE $ stringL (nameBase fName)) $(pure docE) $(pure reqFlag) |]
      argListE <- listE argEs
      [| ToolDefinition $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrE) $(pure argListE) |]

    genPositional conName tys = do
      descrE <- lookupDoc conName
      let argEs = map mkArg (zip [1::Int ..] tys)
            where mkArg (ix,ty) = do
                    let (_,opt)= stripMaybe ty; fStr = "arg_"++show ix ; fName= mkName fStr
                    docE <- lookupDoc fName
                    let reqFlag = ConE (if opt then 'False else 'True)
                    [| ToolArgumentDefinition $(litE $ stringL fStr) $(pure docE) $(pure reqFlag) |]
      argListE <- listE argEs
      [| ToolDefinition $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrE) $(pure argListE) |]

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

instance ToJSON PromptArgumentDefinition where
  toJSON (PromptArgumentDefinition name desc required) = object
    [ fromString "name" .= name
    , fromString "description" .= desc
    , fromString "required" .= required
    ]

instance ToJSON PromptDefinition where
  toJSON (PromptDefinition name desc args) = object
    [ fromString "name" .= name
    , fromString "description" .= desc
    , fromString "arguments" .= args
    ]

instance ToJSON ToolDefinition where
  toJSON (ToolDefinition name desc args) = object
    [ fromString "name"        .= name
    , fromString "description" .= desc
    , fromString "inputSchema" .= toolSchema args
    ]
    where
      toolSchema :: [ToolArgumentDefinition] -> A.Value
      toolSchema as = object
        [ fromString "type"       .= ("object" :: String)
        , fromString "properties" .= object (map argProp as)
        , fromString "required"   .= [ n | ToolArgumentDefinition n _ True <- as ]
        ]
      argProp (ToolArgumentDefinition n d _) =
        (fromString n) .= object
          [ fromString "type"        .= ("string" :: String)
          , fromString "description" .= d
          ]
