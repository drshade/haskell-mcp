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

snakeCase :: String -> String
snakeCase []     = []
snakeCase (c:cs) = toLower c : go cs
  where
    go (x:xs) | isUpper x = '_' : toLower x : go xs
    go (x:xs)            = x : go xs
    go []                = []

renderTy :: Type -> String
renderTy = \case
  ConT n        -> nameBase n
  VarT n        -> nameBase n
  AppT l r      -> renderTy l ++ " " ++ renderTy r
  SigT t _      -> renderTy t
  _             -> "<complex>"

stripMaybe :: Type -> (Type, Bool)
stripMaybe (AppT (ConT n) t) | n == ''Maybe = (t, True)
stripMaybe t                                = (t, False)

lookupArg :: String -> [ArgumentInvocation] -> Maybe String
lookupArg key = go
  where
    go []                                           = Nothing
    go (MkArgumentInvocation k v : xs) | k == key   = Just v
                                     | otherwise    = go xs

derivePrompts :: Name -> Name -> Q Exp
derivePrompts tyName tblName =
  let lookupDoc nm = let s = nameBase nm in [| M.findWithDefault ("<no doc for " ++ s ++ ">") s $(varE tblName) |]
  in derivePrompts' tyName lookupDoc

deriveTools :: Name -> Name -> Q Exp
deriveTools tyName tblName =
  let lookupDoc nm = let s = nameBase nm in [| M.findWithDefault ("<no doc for " ++ s ++ ">") s $(varE tblName) |]
  in deriveTools' tyName lookupDoc

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

deriveInvokeGeneric
  :: Name   -- ^ ADT to reflect (''PromptInstance or ''ToolInstance)
  -> Name   -- ^ Invocation *type* name (''PromptInvocation)
  -> Name   -- ^ Invocation constructor ('MkPromptInvocation)
  -> String -- ^ Human label for error msg ("PromptDefinition" / "ToolDefinition")
  -> Q [Dec]
deriveInvokeGeneric adtName invTypeName invConName errLabel = do
  TyConI (DataD _ _ _ _ cons _) <- reify adtName

  matchClauses   <- mapM mkClause cons
  fallbackClause <- mkFallback
  let clauses = matchClauses ++ [fallbackClause]

  let funName = mkName ("invoke" ++ nameBase adtName)
      funType = AppT (AppT ArrowT (ConT invTypeName))
                     (AppT (AppT (ConT ''Either) (ConT ''String)) (ConT adtName))
  pure [ SigD funName funType
       , FunD funName clauses ]
  where
    ------------------------------------------------------------------
    mkClause :: Con -> Q Clause
    mkClause con = case con of
      RecC cName fields -> mkClauseWithFields cName fields
      NormalC cName []  -> mkClauseWithFields cName []      -- argument-less constructor
      NormalC cName tys -> mkClauseWithFields cName (map (\t -> (mkName "_", Bang NoSourceUnpackedness NoSourceStrictness, snd t)) tys)
      _ -> fail $ "deriveInvokeGeneric: unsupported constructor " ++ show con

    -- helper that builds the clause given a name and (possibly empty) fields list
    mkClauseWithFields :: Name -> [VarBangType] -> Q Clause
    mkClauseWithFields cName fields = do
      let patTag = LitP (StringL (snakeCase (nameBase cName)))
      argsName <- newName "_args"
      bodyExp  <- buildBody cName fields (VarE argsName)
      pure $ Clause [ConP invConName [] [patTag, VarP argsName]] (NormalB bodyExp) []

    ------------------------------------------------------------------
    mkFallback :: Q Clause
    mkFallback = do
      pid <- newName "p"
      as  <- newName "a"
      err <- [| Left ("Unable to match " ++ $(varE pid) ++ " to any " ++ errLabel ++ " (args " ++ show $(varE as) ++ ")") |]
      pure $ Clause [ConP invConName [] [VarP pid, VarP as]] (NormalB err) []

    ------------------------------------------------------------------
    buildBody :: Name -> [VarBangType] -> Exp -> Q Exp
    buildBody conName fields argsExp = do
      let mkField (fName, _, fTy) =
            case stripMaybe fTy of
              (_, True ) -> [| Right (lookupArg $(litE $ stringL $ nameBase fName) $(pure argsExp)) |]
              (_, False) -> [| maybe (Left ("missing field " ++ $(litE $ stringL $ nameBase fName)))
                                   Right
                                   (lookupArg $(litE $ stringL $ nameBase fName) $(pure argsExp)) |]
          fieldQs :: [ExpQ]
          fieldQs = map mkField fields
          startQ  = [| Right $(conE conName) |]
          chain acc nxt = [| $acc <*> $nxt |]
      foldl chain startQ fieldQs

deriveInvokePrompt :: Name -> Q [Dec]
deriveInvokePrompt ty = deriveInvokeGeneric ty ''PromptInvocation 'MkPromptInvocation "PromptDefinition"

deriveInvokeTool :: Name -> Q [Dec]
deriveInvokeTool ty   = deriveInvokeGeneric ty ''ToolInvocation 'MkToolInvocation "ToolDefinition"
