module Language.While.Pretty
       ( prettyPrint
       , Pretty
       ) where

import Language.While.Syntax

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

parensIf :: Bool -> Doc -> Doc
parensIf False = id
parensIf True = PP.parens

class Pretty p where
  printDoc :: Int -> p -> Doc

instance Pretty AExp where
  printDoc _ (Lit n) = PP.int n
  printDoc _ (Var name) = PP.text name
  printDoc n (Minus a b) =
    parensIf (n > 0) (printDoc (n+1) a <+> PP.text " - " <+> printDoc (n+1) b)
  printDoc n (Multiply a b) =
    parensIf (n > 0) (printDoc (n+1) a <+> PP.text " * " <+> printDoc (n+1) b)

instance Pretty BExp where
  printDoc _ (Tr) = PP.text "true"
  printDoc n (LEQ a b) =
    parensIf (n > 0) (printDoc (n+1) a <+> PP.text " <= " <+> printDoc (n+1) b)
  printDoc n (Not b) =
    parensIf (n > 0) (PP.text "not " <+> printDoc (n+1) b)
  printDoc n (And a b) =
    parensIf (n > 0) (printDoc (n+1) a <+> PP.text " && " <+> printDoc (n+1) b)

instance Pretty Com where
  printDoc _ Skip = PP.text "skip"
  printDoc _ (Assignment name a) =
    PP.text name <+> PP.text ":=" <+> printDoc 0 a
  printDoc _ (Sequence c1 c2) =
    printDoc 0 c1 <+> PP.text ";" <+> printDoc 0 c2
  printDoc _ (If b t e) =
    PP.text "if"
    <+> PP.parens (printDoc 0 b)
    <+> PP.text "then"
    <+> printDoc 0 t
    <+> PP.text "else"
    <+> printDoc 0 e
  printDoc _ (While b c) =
    PP.text "while"
    <+> PP.parens (printDoc 0 b)
    <+> PP.text "do"
    <+> printDoc 0 c

prettyPrint :: Pretty a => a -> String
prettyPrint = PP.render . printDoc 0
