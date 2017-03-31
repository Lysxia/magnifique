module Magnifique.Internal where

import Data.Char
import Data.Foldable
import Data.List
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple.Internal

data Zipper = Zipper Context Expr
  deriving Show

type Context = [Co]

data BBP = Brackets' | Braces' | Parens'
  deriving Show

data Co
  = Co BBP [[Expr]] [Expr] [Expr] [[Expr]]
  deriving Show

_unzip :: Expr -> Zipper
_unzip = Zipper []

_zip :: Zipper -> Expr
_zip (Zipper ctx e) = foldl' rebuild e ctx
  where
    rebuild e (Co bbp lf le re rf) = coco bbp lf le e re rf

coco :: BBP -> [[Expr]] -> [Expr] -> Expr -> [Expr] -> [[Expr]] -> Expr
coco bbp lf le e re rf =
  (coco' bbp . CommaSeparated) (reverse lf ++ [reverse le ++ [e] ++ re] ++ rf)

coco' :: BBP -> CommaSeparated [Expr] -> Expr
coco' Brackets' = Brackets
coco' Braces' = Braces
coco' Parens' = Parens

moveUp :: Zipper -> Maybe Zipper
moveUp (Zipper ctx e) = case ctx of
  Co bbp lf le re rf : ctx ->
    Just (Zipper ctx (coco bbp lf le e re rf))
  [] -> Nothing

moveDown :: Zipper -> Maybe Zipper
moveDown (Zipper ctx e) = case e of
  Brackets (CommaSeparated ((e : re) : rf)) ->
    Just (Zipper (Co Brackets' [] [] re rf : ctx) e)
  Braces (CommaSeparated ((e : re) : rf)) ->
    Just (Zipper (Co Braces' [] [] re rf : ctx) e)
  Parens (CommaSeparated ((e : re) : rf)) ->
    Just (Zipper (Co Parens' [] [] re rf : ctx) e)
  _ -> Nothing

moveLeft :: Zipper -> Maybe Zipper
moveLeft (Zipper ctx e) = case ctx of
  Co bbp lf le re rf : ctx ->
    let
      go lf (e' : le) re rf = Just (Zipper (Co bbp lf le re rf : ctx) e')
      go (le : lf) [] re rf = go lf le [] (re : rf)
      go [] [] _ _ = Nothing
    in
      go lf le (e : re) rf
  [] -> Nothing

moveRight :: Zipper -> Maybe Zipper
moveRight (Zipper ctx e) = case ctx of
  Co bbp lf le re rf : ctx ->
    let
      go lf le (e' : re) rf = Just (Zipper (Co bbp lf le re rf : ctx) e')
      go lf le [] (re : rf) = go (le : lf) [] re rf
      go _ _ [] [] = Nothing
    in
      go lf (e : le) re rf
  [] -> Nothing

cozip :: Zipper -> Expr
cozip (Zipper ctx e) = foldl' rebuild (root e) ctx
  where
    root (Brackets _) = Other ">[_]<"
    root (Braces _) = Other ">{_}<"
    root (Parens _) = Other ">(_)<"
    root (StringLit s) = Other $ ">" ++ show s ++ "<"
    root (Other s) = Other $ clean s
    rebuild e (Co bbp lf le re rf) =
      coco bbp (fmap silence lf) (silence le) e (silence re) (fmap silence rf)
    silence = fmap (const (Other "_"))

clean :: String -> String
clean s = f "<" (f ">" s)
  where f c = reverse . (\(a, b) -> a ++ c ++ b) . span isSpace

exprToText :: Expr -> Text
exprToText = toStrict . render defaultOutputOptionsNoColor . toList . expressionsToOutputs . return
