{-# LANGUAGE OverloadedStrings #-}
module HW3.Pretty
  ( prettyValue
  ) where

import           HW3.Base

import qualified Data.ByteString               as BS
import           Data.Foldable                 (toList)
import qualified Data.Map                      as Map
import           Data.Ratio                    (denominator, numerator)
import           Data.Scientific               (FPFormat (..), formatScientific,
                                                fromRationalRepetendUnlimited)
import qualified Data.Sequence                 as Seq
import           Data.Word                     (Word8)
import           Numeric                       (showHex)
import           Prettyprinter                 (Doc, Pretty (..), annotate,
                                                enclose, encloseSep,
                                                unsafeViaShow, viaShow, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Prettyprinter.Symbols.Ascii   (colon, comma, dquote, lbrace,
                                                lbracket, lparen, rbrace,
                                                rbracket, rparen, space)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num) = pretty (prettyRational num)
prettyValue (HiValueFunction fun) = unsafeViaShow fun
prettyValue (HiValueBool True) = "true"
prettyValue (HiValueBool False) = "false"
prettyValue (HiValueNull) = "null"
prettyValue (HiValueString text) = viaShow text

prettyRational :: Rational -> String
prettyRational rational =
  case fromRationalRepetendUnlimited rational of
    (scientific, Nothing)  -> case quotRem' of
      (q, 0) -> show q
      _      -> formatScientific Fixed Nothing scientific
    (_, Just _) -> case truncate rational of
      0                -> show (numerator rational) ++ "/" ++ show (denominator rational)
      _ | rational > 0 -> show (fst quotRem') ++ " + " ++ show (snd quotRem') ++ "/" ++ show (denominator rational)
      _                -> show (fst quotRem') ++ " - " ++ show (abs $ snd quotRem') ++ "/" ++ show (denominator rational) 
    where
       quotRem' = quotRem (numerator rational) (denominator rational) 
       
 