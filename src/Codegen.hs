{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen where

import qualified Data.Map as Map
import qualified Data.String as String

import Control.Monad.State

import qualified LLVM.IRBuilder.Module          as L
import qualified LLVM.IRBuilder.Monad           as L
import qualified LLVM.IRBuilder.Instruction     as L
import qualified LLVM.IRBuilder.Constant        as L
import qualified LLVM.Context                   as L
import qualified LLVM.AST                       as L (Module(..))
import qualified LLVM.AST.Type                  as L
import qualified LLVM.AST.Name                  as L
import qualified LLVM.AST.Operand               as L
import qualified LLVM.AST.Constant              as L
import qualified LLVM.AST.IntegerPredicate      as L
import qualified LLVM.AST.Float                 as L

import Control.Monad

import qualified Syntax                         as S

type ModuleBuilder = L.ModuleBuilderT Codegen
type IRBuilder = L.IRBuilderT ModuleBuilder

data CodegenState =
  CodegenState
    { symbols :: Map.Map S.Name L.Operand
    , globalStrings :: Map.Map String S.Name
    }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadFix)

addSym :: S.Name -> L.Operand -> Codegen ()
addSym name addr = do
    syms <- gets symbols
    modify $ \s -> s { symbols = Map.insert name addr syms }
    return ()

getOrCreateString :: String -> ModuleBuilder L.Operand
getOrCreateString str = do
  globals <- gets globalStrings
  syms <- gets symbols
  let name = case Map.lookup str globals of
                Just name -> name
                Nothing -> "g" ++ show (Map.size globals)
  case Map.lookup name syms of
    Just addr -> return addr
    Nothing -> do
      addr <- L.globalStringPtr str (L.mkName name)
      modify $ \s ->
        s
          { symbols = Map.insert name (L.ConstantOperand addr) syms
          , globalStrings = Map.insert str name globals
          }
      return $ L.ConstantOperand addr

codegenExpression :: S.Expression -> IRBuilder L.Operand
codegenExpression (S.Integer n) = return $ L.ConstantOperand (L.Int 64 n)
codegenExpression (S.Double n) = return $ L.ConstantOperand (L.Float (L.Double n))
codegenExpression (S.String s) = lift $ getOrCreateString s
codegenExpression (S.Variable x) = getvar x
  where
    getvar :: S.Name -> IRBuilder L.Operand
    getvar var = do
      syms <- gets symbols
      case Map.lookup var syms of
        Just x  -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var

codegenExpression (S.Call fn args) = do
  largs <- mapM codegenExpression args
  addr <- getaddr fn
  L.call addr (callargs largs)
  where
    callargs = map $ \a -> (a, [])
    getaddr :: S.Name -> IRBuilder L.Operand
    getaddr name = do
      syms <- gets symbols
      case Map.lookup name syms of
        Just x -> return x
        Nothing -> error $ "Function not defined : " ++ show name

codegenExpression (S.BinaryOperator op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- codegenExpression a
      cb <- codegenExpression b
      f ca cb
    Nothing -> error $ "No such operator" ++ show op
  where
    binops =
      Map.fromList
        [("+", L.add), ("-", L.sub), ("*", L.mul), ("/", L.udiv),
         ("==", L.icmp L.EQ), ("<", L.icmp L.SLT), (">", L.icmp L.SGT)
        ]

codegenExpression (S.Conditional cexp exp1 exp2) = mdo
    cond <- codegenExpression cexp
    L.condBr cond trueBlock falseBlock
    trueBlock <- L.block
    trueRet <- codegenExpression exp1
    L.br exitBlock
    falseBlock <- L.block
    falseRet <- codegenExpression exp2
    L.br exitBlock
    exitBlock <- L.block
    r <- L.phi [(trueRet, trueBlock), (falseRet, falseBlock)]
    return r

codegenDefinition :: S.Definition -> ModuleBuilder L.Operand
codegenDefinition (S.Function name args body) = mdo
    addr <- L.function (L.mkName name) argtys L.i64 $ \ops -> do
        forM_ (zip ops args) $ \(op, arg) -> do
            syms <- gets symbols
            modify $ \s -> s { symbols = Map.insert arg op syms }
        lift $ lift $ addSym name addr
        rv <- codegenExpression body
        L.ret rv
    return addr
    where
        argtys = flip map args $ \a ->
            (L.i64, String.fromString a)

codegenDefinition (S.Extern name args) = do
    addr <- L.extern (L.mkName name) [L.i64] L.i64
    lift $ addSym name addr
    return addr

codegenModule :: S.Module -> Codegen L.Module
codegenModule mod = L.buildModuleT "main" $ mapM_ codegenDefinition mod

codegen :: S.Module -> L.Module
codegen mod =
  evalState
    (runCodegen $ codegenModule mod)
    CodegenState {symbols = Map.fromList [], globalStrings = Map.fromList []}
