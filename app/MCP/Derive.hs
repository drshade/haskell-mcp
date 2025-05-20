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
import           Data.List                  (foldl')
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

data ArgumentDefinition = ArgumentDefinition ArgumentName ArgumentDescription ArgumentRequired
  deriving (Show)

data PromptDefinition = PromptDefinition String String [ArgumentDefinition]
  deriving (Show)

data ArgumentInvocation = ArgumentInvocation String String
  deriving (Show)

data PromptInvocation = PromptInvocation String [ArgumentInvocation]
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
    go (ArgumentInvocation k v : xs) | k == key = Just v
                                     | otherwise = go xs


-------------------------------------------------------------------------------
-- Public API 1 – no doc table
-------------------------------------------------------------------------------

derivePrompts :: Name -> Q Exp
derivePrompts tyName = deriveN tyName (\_ -> [| "" :: String |])

-------------------------------------------------------------------------------
-- Public API 2 – with a value‑level DocTable
-------------------------------------------------------------------------------

derivePromptsWithDocs :: Name -> Name -> Q Exp
derivePromptsWithDocs tyName tblName =
  let lookupDoc nm =
        let nmStr = nameBase nm in
        [| M.findWithDefault
             $(litE $ stringL ("<no doc for " ++ nmStr ++ ">"))
             nmStr
             $(varE tblName) |]
  in deriveN tyName lookupDoc

-------------------------------------------------------------------------------
-- Core generator shared by both front‑ends
-------------------------------------------------------------------------------

deriveN :: Name            -- ^ datatype to reflect
        -> (Name -> Q Exp) -- ^ doc lookup: key → TH Exp that yields String
        -> Q Exp
deriveN tyName lookupDoc = do
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
                  let (innerTy, optional) = stripMaybe fTy
                      promptId            = snakeCase (nameBase fName)
                  docE <- lookupDoc fName
                  let reqFlag = ConE (if optional then 'False else 'True)
                  [| ArgumentDefinition $(litE $ stringL promptId)
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
                  let (innerTy, optional) = stripMaybe ty
                      fStr                = "arg_" ++ show ix
                      fName               = mkName fStr
                  docE <- lookupDoc fName
                  let reqFlag = ConE (if optional then 'False else 'True)
                  [| ArgumentDefinition $(litE $ stringL fStr)
                                     $(pure docE)
                                     $(pure reqFlag) |]
      argListE <- listE argEs
      [| PromptDefinition $(litE $ stringL (snakeCase (nameBase conName))) $(pure descrE) $(pure argListE) |]

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
      errExp <- [| Left ("Unable to match " ++ $(varE pidVar) ++ " to any PromptDefinition") |]
      pure $ Clause [ConP 'PromptInvocation [] [VarP pidVar, VarP argsVar]] (NormalB errExp) []

    ----------------------------------------------------------------
    buildBody :: Name -> [VarBangType] -> Exp -> Q Exp
    ----------------------------------------------------------------
    buildBody conName fields argsExp = do
      let mkField :: (Name, Bang, Type) -> Q Exp
          mkField (fName, _, fTy) =
            case stripMaybe fTy of
              (inner, True ) ->  -- optional
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
