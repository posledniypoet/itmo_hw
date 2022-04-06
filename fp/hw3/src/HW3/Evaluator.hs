{-# LANGUAGE PartialTypeSignatures #-}

module HW3.Evaluator where

import Control.Exception.Base (throw)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.Ratio (denominator, numerator, (%))
import Data.Semigroup (stimes)
import Data.Text (Text, pack, reverse, strip, toLower, toUpper, unpack)
import HW3.Base
import Prelude hiding (reverse)

data MyType = Rational | Text

slice :: Int -> Int -> String -> String
slice from to xs = take (to - from + 1) (drop from xs)

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

eval' :: Monad m => HiExpr -> ExceptT HiError m HiValue
eval' e = case e of
  HiExprValue val -> return val
  HiExprApply f args -> eHiExprApply f args

eHiExprApply :: Monad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
eHiExprApply func args = case func of
  HiExprValue value -> case value of
    HiValueBool bool -> throwE HiErrorInvalidFunction
    HiValueNumber num -> throwE HiErrorInvalidFunction
    HiValueString str -> applyStringSlice str args
    HiValueFunction func' -> case func' of
      HiFunDiv -> applyBinary HiFunDiv args
      HiFunMul -> applyBinary HiFunMul args
      HiFunAdd -> applyBinary HiFunAdd args
      HiFunSub -> applyBinary HiFunSub args
      HiFunLessThan -> applyLog (<) args
      HiFunGreaterThan -> applyLog (>) args
      HiFunNotLessThan -> applyLog (>=) args
      HiFunNotGreaterThan -> applyLog (<=) args
      HiFunEquals -> applyLog (==) args
      HiFunNotEquals -> applyLog (/=) args
      HiFunAnd -> applyB (&&) args
      HiFunOr -> applyB (||) args
      HiFunNot -> applyUnary not args
      HiFunIf -> applyTernary args
      HiFunLength -> applyLength length args
      HiFunToUpper -> applyString toUpper args
      HiFunToLower -> applyString toLower args
      HiFunTrim -> applyString strip args
      HiFunReverse -> applyString reverse args
      HiFunList -> undefined
      _ -> throwE HiErrorInvalidFunction
  HiExprApply func' args' -> do
    x <- eHiExprApply func' args'
    eHiExprApply (HiExprValue x) args


applyStringSlice :: Monad m => Text -> [HiExpr] -> ExceptT HiError m HiValue
applyStringSlice str [e1] = do
  x <- eval' e1
  case x of
    (HiValueNumber x') ->
      if ((denominator x' == 1) && (x' >= 0) && (x' < 0 || x' >= fromIntegral (length (unpack str))))|| x' <0
        then return HiValueNull
        else
          if (denominator x' == 1) && (x' >= 0)
            then return $ HiValueString $ pack (slice (fromIntegral (numerator x')) (fromIntegral (numerator x')) (unpack str))
            else throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument
applyStringSlice str [e1, e2] = do
  x <- eval' e1
  y <- eval' e2
  case (x, y) of
    (HiValueNumber x', HiValueNumber y') ->
      if (denominator x' == 1) && (x' >= 0) && (denominator y' == 1) && (y' >= 0)
        then return $ HiValueString $ pack (slice (fromIntegral (numerator x')) (fromIntegral (numerator y')) (unpack str))
        else throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument
applyStringSlice _ _ = throwE HiErrorArityMismatch

applyTernary :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
applyTernary [e1, e2, e3] = do
  x <- eval' e1
  y <- eval' e2
  z <- eval' e3
  case (x, y, z) of
    (HiValueBool x', y', z') -> if x' then return y' else return z'
    _ -> throwE HiErrorInvalidArgument
applyTernary _ = throwE HiErrorArityMismatch

applyString :: Monad m => (Text -> Text) -> [HiExpr] -> ExceptT HiError m HiValue
applyString fun [e1] = do
  x <- eval' e1
  case x of
    (HiValueString x') -> return $ HiValueString $ fun x'
    _ -> throwE HiErrorInvalidArgument
applyString _ _ = throwE HiErrorArityMismatch

applyLength :: Monad m => ([Char] -> Int) -> [HiExpr] -> ExceptT HiError m HiValue
applyLength fun [e1] = do
  x <- eval' e1
  case x of
    (HiValueString x') -> return $ HiValueNumber $ toRational (fun (unpack x'))
    _ -> throwE HiErrorInvalidArgument
applyLength _ _ = throwE HiErrorArityMismatch

applyUnary :: Monad m => (Bool -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
applyUnary fun [e1] = do
  x <- eval' e1
  case x of
    (HiValueBool x') -> return $ HiValueBool $ fun x'
    _ -> throwE HiErrorInvalidArgument
applyUnary _ _ = throwE HiErrorArityMismatch

applyB :: Monad m => (Bool -> Bool -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
applyB fun [e1, e2] = do
  x <- eval' e1
  y <- eval' e2
  case (x, y) of
    (HiValueBool x', HiValueBool y') -> return $ HiValueBool $ fun x' y'
    _ -> throwE HiErrorInvalidArgument
applyB _ _ = throwE HiErrorArityMismatch

applyLog :: Monad m => (HiValue -> HiValue -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
applyLog fun [e1, e2] = do
  x <- eval' e1
  y <- eval' e2
  case (x, y) of
    (x', y') -> return $ HiValueBool $ fun y x
    _ -> throwE HiErrorInvalidArgument
applyLog _ _ = throwE HiErrorArityMismatch

applyBinary :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
applyBinary fun [e1, e2] = do
  x <- eval' e1
  y <- eval' e2
  case fun of
    HiFunAdd -> case (x, y) of
      (HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ (+) x' y'
      (HiValueString x', HiValueString y') -> return $ HiValueString $ pack ((++) (unpack x') (unpack y'))
      _ -> throwE HiErrorInvalidArgument
    HiFunDiv -> case (x, y) of
      (HiValueNumber x', HiValueNumber y') -> if y' == 0 then throwE HiErrorDivideByZero else return $ HiValueNumber $ (/) x' y'
      (HiValueString x', HiValueString y') -> return $ HiValueString $ pack ((++) ((++) (unpack x') "/") (unpack y'))
      _ -> throwE HiErrorInvalidArgument
    HiFunMul -> case (x, y) of
      (HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ (*) x' y'
      (HiValueString x', HiValueNumber y') -> if (denominator y' == 1) && (y' >= 0) then return $ HiValueString $ pack (stimes (numerator y') (unpack x')) else throwE HiErrorInvalidArgument
      _ -> throwE HiErrorInvalidArgument
    HiFunSub -> case (x, y) of
      (HiValueNumber x', HiValueNumber y') -> return $ HiValueNumber $ (-) x' y'
      _ -> throwE HiErrorInvalidArgument
applyBinary _ _ = throwE HiErrorArityMismatch
