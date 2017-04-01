module Magnifique.App.Aeson where

import Brick
import Graphics.Vty (Key(..), Event(..), defAttr)
import Magnifique.Aeson
import Magnifique.App.Common

magnifiqueApp :: App Zipper () String
magnifiqueApp = App
  { appDraw = \s ->
      [txt (zipperToText s)]
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent = \s e -> case e of
      VtyEvent e -> case e of
        EvKey KUp [] -> continue' (moveUp s)
        EvKey KDown [] -> continue' (moveDown s)
        EvKey KLeft [] -> continue' (moveLeft s)
        EvKey KRight [] -> continue' (moveRight s)
        EvKey (KChar 'q') [] -> halt s
       where continue' Nothing = continue s
             continue' (Just s) = continue s
      _ -> continue s
  , appStartEvent = return
  , appAttrMap = magnifiqueAttrMap
  }
