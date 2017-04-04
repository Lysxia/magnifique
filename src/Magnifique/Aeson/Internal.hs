{-# LANGUAGE OverloadedStrings #-}

module Magnifique.Aeson.Internal where

import Data.Aeson (Value(..))
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Text as Text
import qualified Data.Vector as V

data Zipper = Zipper Context Value

type Context = [Co]

data Co
  = CoObject [(Text, Value)] Text [(Text, Value)]
  | CoArray [Value] [Value]

_unzip :: Value -> Zipper
_unzip = Zipper []

moveUp :: Zipper -> Maybe Zipper
moveUp (Zipper co v) = case co of
  CoObject l k r : co ->
    Just (Zipper co (Object (HMap.fromList (l ++ [(k, v)] ++ r))))
  CoArray l r : co ->
    Just (Zipper co (Array (V.fromList (l ++ [v] ++ r))))
  _ -> Nothing

moveDown :: Zipper -> Maybe Zipper
moveDown (Zipper co v) = case v of
  Object o | (k, v) : o <- sortOn fst (HMap.toList o) ->
    Just (Zipper (CoObject [] k o : co) v)
  Array a | v : a <- V.toList a ->
    Just (Zipper (CoArray [] a : co) v)
  _ -> Nothing

moveLeft :: Zipper -> Maybe Zipper
moveLeft (Zipper co v) = case co of
  CoObject ((k', v') : l) k r : co ->
    Just (Zipper (CoObject l k' ((k, v) : r) : co) v')
  CoArray (v' : l) r : co ->
    Just (Zipper (CoArray l (v : r) : co) v')
  _ -> Nothing

moveRight :: Zipper -> Maybe Zipper
moveRight (Zipper co v) = case co of
  CoObject l k ((k', v') : r) : co ->
    Just (Zipper (CoObject ((k, v) : l) k' r : co) v')
  CoArray l (v' : r) : co ->
    Just (Zipper (CoArray (v : l) r : co) v')
  _ -> Nothing

coToText :: Int -> Co -> Text
coToText n (CoObject l k r) =
  let (l1, l2) = splitAt n l
      (r1, r2) = splitAt n r
  in
    Text.concat
      [ "{", showEllipsis l2, comma l2, showKeys l1, comma l1
      , showFocusKey k
      , comma r1, showKeys r1, comma r2, showEllipsis r2, "}"
      ]
  where
    showKeys [] = ""
    showKeys ks = Text.intercalate ","
      [Text.concat [Text.pack (show k), ":", ellipsis] | (k, _) <- ks]
    showFocusKey k = Text.concat [Text.pack (show k), ":", focusText]
coToText _ (CoArray l r) =
  Text.concat
    ["[", showEllipsis l, comma l, focusText, comma r, showEllipsis r, "]"]

ellipsis :: Text
ellipsis = "_"

focusText :: Text
focusText = "!"

showEllipsis :: [a] -> Text
showEllipsis [] = ""
showEllipsis l = Text.concat [ellipsis, "(", Text.pack (show (length l)), ")"]

comma :: [a] -> Text
comma [] = ""
comma _ = ","

showRoot :: Value -> Text
showRoot (Object o)
  | HMap.null o = "{}"
  | otherwise = "{...}"
showRoot (Array a)
  | V.null a = "[]"
  | otherwise = "[...]"
showRoot (String t) = Text.pack (show t)
showRoot (Number n) = Text.pack (show n)
showRoot (Bool b) = Text.pack (show b)
showRoot Null = "null"

zipperToText :: Zipper -> Text
zipperToText (Zipper co v) =
  Text.unlines (reverse (showRoot v : fmap (coToText 2) co))
